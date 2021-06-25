
# limpando o ambiente -------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -----------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(DiagrammeR)
library(ggridges)
library(GGally)
library(skimr)
library(piecewiseSEM)

# carregando os dados -------------------------------------------------------------------------------------------------------------------------------------

## dados originais
originais <- read_rds(file = 'data/dados.rds')

# tratando os dados ---------------------------------------------------------------------------------------------------------------------------------------

# selecionando apenas as colunas necessarias
dados <- originais %>% 
  select(ranking, nome, nota_geral, nota_media, n_notas, ano_criacao, disponivel_br, ano_lancamento_br, contains('pessoas_'))

# tempo de criação
dados <- dados %>% 
  mutate(delta_nota = nota_media - nota_geral,
         tempo_criacao = year(Sys.Date()) - ano_criacao,
         tempo_lancamento_br = year(Sys.Date()) - as.numeric(ano_lancamento_br),
         n_notas_criacao = case_when(tempo_criacao == 0 ~ n_notas,
                                     TRUE ~ n_notas / tempo_criacao),
         n_notas_lancamento = case_when(tempo_lancamento_br == 0 ~ n_notas,
                                        TRUE ~ n_notas / tempo_lancamento_br)) %>% 
  select(-ano_criacao, -ano_lancamento_br) %>% 
  filter(tempo_criacao < 100)

# analise exploratoria ------------------------------------------------------------------------------------------------------------------------------------

# skim dos dados
skim(data = dados)

## relação par a par
ggpairs(data = select(dados, -ranking, -nome, -disponivel_br))

## focando num subconjunto de informações
ggpairs(data = select(dados, pessoas_possuem, n_notas, tempo_criacao, delta_nota, nota_media, nota_geral, disponivel_br))

# distribuicao do delta
dados %>% 
  ggplot(mapping = aes(x = delta_nota)) +
  geom_histogram(bins = 40, color = 'black', fill = 'dodgerblue4') +
  scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(n.breaks = 8) +
  labs(x = 'Diferença entre as notas',
       y = 'Quantidade de Títulos',
       title = 'Diferença entre a nota média e a nota do ranking',
       subtitle = 'A média aritmética da nota é superior à nota do ranking de cada título') +
  theme_minimal()

## todas as variaveis entre estar disponível no brasil ou nao
dados %>% 
  select(tempo_criacao, disponivel_br, pessoas_possuem, n_notas, delta_nota, nota_media, nota_geral) %>% 
  pivot_longer(cols = -disponivel_br, names_to = 'variaveis', values_to = 'valor') %>% 
  ggplot(mapping = aes(x = disponivel_br, y = valor, fill = disponivel_br)) +
  facet_wrap(~ variaveis, scales = 'free') +
  geom_boxplot()

# HIPÓTESE GERAL ------------------------------------------------------------------------------------------------------------------------------------------

## hipótese de diagrama causal
mermaid("
        graph TD
        
        A[Tempo de Criação]
        
        A --> B[Disponível no Brasil]
        
        A-->C[Pessoas Possuem]
        B-->C
        
        A-->D[Qtd. de Notas]
        C-->D
        
        C-->E[Nota Média]
        D-->E
        B-->E
        
        D-->F[Nota Ranking]
        E-->F
        
        style A fill:#FFF, stroke:#333, stroke-width:4px
        style B fill:#9AA, stroke:#9AA, stroke-width:2px
        style C fill:#ADF, stroke:#333, stroke-width:2px
        style D fill:#9C2, stroke:#9C2, stroke-width:2px
        
        ")

## variaveis que exploraremos
# tempo_criacao, disponivel_br, pessoas_possuem, n_notas, delta_nota, nota_media, nota_geral

# modelo causal -------------------------------------------------------------------------------------------------------------------------------------------

## criando algumas variaveis
dados_psem <- dados %>% 
  mutate(disponivel = ifelse(test = disponivel_br == 'Sim', yes = 1L, no = 0L)) %>% 
  select(tempo_criacao, disponivel, pessoas_possuem, n_notas, delta_nota, nota_media, nota_geral) #%>% 
# scale() %>% 
# data.frame %>% 
# as_tibble

# estabelecendo o modelo causal
modelo <- psem(
  # tempo_criacao -> disponibilidade
  glm(disponivel ~ tempo_criacao, data = dados_psem, family = 'binomial'),
  # tempo_criacao + disponivel -> pessoas_possuem
  glm(pessoas_possuem ~ tempo_criacao * disponivel, data = dados_psem, family = 'poisson'),
  # tempo_criacao + pessoas -> notas
  glm(n_notas ~ tempo_criacao * pessoas_possuem * disponivel, data = dados_psem, family = 'poisson'),
  # pessoas + notas + disponivel -> nota_media
  glm(nota_media ~ pessoas_possuem * n_notas, data = dados_psem),
  # nota_media + notas -> nota ranking
  glm(nota_geral ~ nota_media * n_notas, data = dados_psem)
)

# sumario do modelo causal
summary(modelo, standardize = 'scale', test.type = 'III')

# MACHINE LEARNING ----------------------------------------------------------------------------------------------------------------------------------------

## carregando os pacotes
library(tidymodels)
library(tune)

## recodificando a nota media
dados <- dados %>% 
  mutate(quantil_nota = case_when(nota_media <= quantile(x = nota_media, 0.25) ~ 'abaixo',
                                  nota_media >= quantile(x = nota_media, 0.75) ~ 'acima',
                                  TRUE ~ 'mediana'
  )
  )

## separando os dados
train_test_split <- initial_split(data = dados, prop = 0.9, strata = nota_geral)

## criando folds de validacao
skf_data <- vfold_cv(data = training(train_test_split), v = 10, strata = nota_geral)

## regressao linear ----------------------------------------------------------------------------------------------------------------------------------------

## criando receita da regressao
receita_reg <- recipe(nota_geral ~ ., data = training(train_test_split)) %>% 
  step_select(tempo_criacao, disponivel_br, pessoas_possuem, n_notas, quantil_nota, nota_geral) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = FALSE)

## criando o workflow da regressao
algoritmo_reg <- linear_reg() %>% 
  set_engine(engine = 'lm') %>% 
  set_mode('regression')

## setando o workflow do decision tree
modelo_reg <- workflow() %>% 
  add_recipe(receita_reg) %>% 
  add_model(algoritmo_reg)

## fit do modelo
fitted_reg <- fit_resamples(object = modelo_reg, 
                            resamples = skf_data, 
                            metrics = metric_set(rmse, rsq),
                            control = control_resamples(verbose = TRUE, allow_par = TRUE, save_pred = TRUE)
)

## metricas
collect_metrics(fitted_reg)

## treinando modelo final
reg_last_fit <- last_fit(object = modelo_reg, split = train_test_split)

## extraindo metricas do modelo final
collect_metrics(reg_last_fit)

## treinando a regressao
reg_final <- fit(object = modelo_reg, data = dados)

## parametros do modelo
tidy(reg_final)

## metricas
glance(reg_final)

## arvore de decisao ---------------------------------------------------------------------------------------------------------------------------------------

## criando receita da decision tree
receita_dt <- recipe(nota_geral ~ ., data = training(train_test_split)) %>% 
  step_select(tempo_criacao, disponivel_br, pessoas_possuem, n_notas, quantil_nota, nota_geral) %>% 
  step_dummy(all_nominal_predictors(), one_hot = FALSE)

## criando o workflow do decision tree
algoritmo_dt <- decision_tree(tree_depth = tune(), min_n = tune()) %>% 
  set_engine(engine = 'rpart') %>% 
  set_mode(mode = 'regression')

## setando o workflow do decision tree
modelo_dt <- workflow() %>% 
  add_recipe(receita_dt) %>% 
  add_model(algoritmo_dt)

# ## criando tuning para o time
# times <- function(range = c(2L, 50L), trans = NULL) {
#   new_quant_param(
#     type = 'integer', 
#     range = range,
#     inclusive = c(TRUE, TRUE),
#     trans = trans,
#     label = c(times = 'Number of bootstrap samples'),
#     finalize = NULL
#   )
# }

# ## setando os hiperparametros para a busca
hiperparametros <- parameters(modelo_dt) #%>%
#   update(times = times())

## rodando o modelo
doParallel::registerDoParallel()
set.seed(33)
dt_tunning <- modelo_dt %>% 
  tune_bayes(
    resamples = skf_data,
    param_info = hiperparametros,
    initial = 5, 
    iter = 50,
    metrics = metric_set(rmse),
    control = control_bayes(no_improve = 30, verbose = TRUE, seed = 33, save_pred = TRUE)
  )

## coletando as metricas
collect_metrics(x = dt_tunning)

## visualizando os resultados da otimizacação
autoplot(object = dt_tunning)

## pegando o melhor modelo
show_best(x = dt_tunning, metric = 'rmse', n = 5)

## plotando os resultados da otimização
autoplot(object = dt_tunning, type = 'performance', metric = 'rmse')

## extraindo melhor combinação de hiperparametros
hyperpars_best_dt <- select_best(x = dt_tunning, metric = 'rmse')

## instanciando melhoro modelo no final da avaliação
melhor_algoritmo_dt <- finalize_model(x = algoritmo_dt, parameters = hyperpars_best_dt)

## colocando os hiperparametros no workflow
melhor_modelo_dt <- finalize_workflow(x = modelo_dt, parameters = hyperpars_best_dt)
melhor_modelo_dt

## treinando modelo final
dt_last_fit <- last_fit(object = melhor_modelo_dt, split = train_test_split)

## extraindo metricas do modelo final
collect_metrics(dt_last_fit)

## treinando a regressao
dt_final <- fit(object = melhor_modelo_dt, data = dados)

# random forest -------------------------------------------------------------------------------------------------------------------------------------------

## criando o workflow do decision tree
algoritmo_rf <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine(engine = 'ranger', importance = "impurity") %>% 
  set_mode(mode = 'regression')

## setando o workflow do decision tree
modelo_rf <- workflow() %>% 
  add_recipe(receita_dt) %>% 
  add_model(algoritmo_rf)

# ## setando os hiperparametros para a busca
hiperparametros <- parameters(modelo_rf) %>%
  update(mtry = mtry(range = c(1, 5)))

## rodando o modelo
doParallel::registerDoParallel()
set.seed(33)
rf_tunning <- modelo_rf %>% 
  tune_bayes(
    resamples = skf_data,
    param_info = hiperparametros,
    initial = 5, 
    iter = 50,
    metrics = metric_set(rmse),
    control = control_bayes(no_improve = 30, verbose = TRUE, seed = 33, save_pred = TRUE)
  )

## coletando as metricas
collect_metrics(x = rf_tunning)

## visualizando os resultados da otimizacação
autoplot(object = rf_tunning)

## plotando os resultados da otimização
autoplot(object = rf_tunning, type = 'performance', metric = 'rmse')

## pegando o melhor modelo
show_best(x = rf_tunning, metric = 'rmse', n = 5)

## extraindo melhor combinação de hiperparametros
hyperpars_best_rf <- select_best(x = rf_tunning, metric = 'rmse')

## instanciando melhoro modelo no final da avaliação
melhor_algoritmo_rf <- finalize_model(x = algoritmo_rf, parameters = hyperpars_best_rf)

## colocando os hiperparametros no workflow
melhor_modelo_rf <- finalize_workflow(x = modelo_rf, parameters = hyperpars_best_rf)
melhor_modelo_rf

## treinando modelo final
rf_last_fit <- last_fit(object = melhor_modelo_rf, split = train_test_split, metrics = metric_set(rmse, rsq))

## extraindo metricas do modelo final
collect_metrics(rf_last_fit)

## analisando as previsoes
collect_predictions(x = rf_last_fit) %>% 
  ggplot(mapping = aes(x = .pred, y = nota_geral)) +
  geom_abline(slope = 1, linetype = 1, size = 1, color = 'black') +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5) +
  geom_point(shape = 21, fill = 'firebrick3', color = 'black', size = 3) +
  scale_x_continuous(name = 'Valor Predito', n.breaks = 10) +
  scale_y_continuous(name = 'Valor Esperado', n.breaks = 10) +
  theme_classic()

## analisando os residuos
collect_predictions(x = rf_last_fit) %>% 
  mutate(residuo = .pred - nota_geral) %>% 
  ggplot(mapping = aes(x = residuo)) +
  geom_histogram(fill = 'firebrick3', color = 'black', bins = 20) +
  scale_x_continuous(name = 'Resíduo', n.breaks = 10) +
  scale_y_continuous(name = 'Frequência', n.breaks = 10) +
  theme_classic()

## treinando a random forest
rf_final <- fit(object = melhor_modelo_rf, data = dados)

## importancia das variaveis
rf_last_fit %>% 
  pluck('.workflow', 1) %>% 
  pull_workflow_fit() %>% 
  vip::vip()

