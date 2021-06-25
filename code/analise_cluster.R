
# limpando o ambiente -------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando os pacotes -----------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cluster)
library(vegan)
library(plotly)
library(ggdendro)
library(Rtsne)
library(FD)

# carregando os dados -------------------------------------------------------------------------------------------------------------------------------------

## matriz pré-formatada
dados <- read_rds(file = 'data/matriz.rds')

## dados originais
originais <- read_rds(file = 'data/dados.rds')

# calculando a distancia de gower -------------------------------------------------------------------------------------------------------------------------

## criando a matriz base
base_df <- dados %>% 
  select(-contains('Indeterminado')) %>% 
  mutate(across(where(is.logical), as.numeric)) %>% 
  data.frame %>% 
  `rownames<-`(value = .$nome) %>% 
  select(-nome)

## matriz de gower par a par para todas as características
gower_all <- gowdis(x = base_df, ord = 'podani')

# subset das informações mais uteis
gower_subset <- base_df %>% 
  select(contains('mecanica'), contains('tema'), complexidade) %>% 
  gowdis(x = ., ord = 'podani')

# somente as informacoes que fizemos one hot encoding
gower_ohe <- base_df %>% 
  select(contains('mecanica'), contains('tema'), contains('categoria')) %>% 
  gowdis(x = ., ord = 'podani')

# somente as informacoes de mecanica e tema
gower_mec_tema <- base_df %>% 
  select(contains('mecanica'), contains('tema')) %>% 
  gowdis(x = ., ord = 'podani')

# DISTRIBUIÇÃO DOS VALORES DE DISSIMILARIDADE -------------------------------------------------------------------------------------------------------------

# copiando a matriz
gower_dist <- as.matrix(gower_subset)

# retendo so a diagonal inferior
gower_dist[upper.tri(x = gower_dist, diag = TRUE)] <- NA

# passando tudo para formato longo
gower_dist %>% 
  as.data.frame() %>% 
  rownames_to_column(var = 'titulo') %>% 
  as_tibble() %>% 
  pivot_longer(cols = -titulo, names_to = 'outro_titulo', values_to = 'dissimilaridade') %>% 
  drop_na() %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = dissimilaridade), bins = 60, color = 'black', fill = 'dodgerblue3') +
  scale_x_continuous(breaks = c(seq(from = 0, to = 0.3, by = 0.05), seq(from = 0.4, to = 1, by = 0.2)),
                     limits = c(0, 1.05))

# PARTITIONING WITH MEDOIDS -------------------------------------------------------------------------------------------------------------------------------
# criando matriz com os valores de K ----------------------------------------------------------------------------------------------------------------------

## criando uma matriz para armazenar os valores de K
pam_k_values <- tibble(
  K = seq(from = 2, to = 250, by = 1)
)

# rodando o PAM -------------------------------------------------------------------------------------------------------------------------------------------

## mapeando o algoritmo a cada valor de K
pam_k_values <- pam_k_values %>% 
  mutate(pam_all = map(.x = K, ~ pam(x = gower_all, k = .x, diss = TRUE)),
         pam_subset = map(.x = K, ~ pam(x = gower_subset, k = .x, diss = TRUE)),
         pam_ohe = map(.x = K, ~ pam(x = gower_ohe, k = .x, diss = TRUE)),
         pam_mec_tema = map(.x = K, ~ pam(x = gower_mec_tema, k = .x, diss = TRUE))
  )

## calculando o valor da silhueta para cada valor de K
pam_k_values <- pam_k_values %>% 
  mutate(sil_all = map_dbl(.x = pam_all, ~ .$silinfo$avg.width),
         sil_subset = map_dbl(.x = pam_subset, ~ .$silinfo$avg.width),
         sil_ohe = map_dbl(.x = pam_ohe, ~ .$silinfo$avg.width),
         sil_mec_tema = map_dbl(.x = pam_mec_tema, ~ .$silinfo$avg.width)
  )

## extraindo os medoides
pam_k_values <- pam_k_values %>% 
  mutate(med_all = map(.x = pam_all, ~ .$medoids),
         med_subset = map(.x = pam_subset, ~ .$medoids),
         med_ohe = map(.x = pam_ohe, ~ .$medoids),
         med_mec_tema = map(.x = pam_mec_tema, ~ .$medoids)
  )

## extraindo os clusteres de cada jogo
pam_k_values <- pam_k_values %>% 
  mutate(clustering_all = map(.x = pam_all, ~ .$clustering %>% enframe(name = 'nome', value = 'cluster_id')),
         clustering_subset = map(.x = pam_subset, ~ .$clustering %>% enframe(name = 'nome', value = 'cluster_id')),
         clustering_ohe = map(.x = pam_ohe, ~ .$clustering %>% enframe(name = 'nome', value = 'cluster_id')),
         clustering_mec_tema = map(.x = pam_mec_tema, ~ .$clustering %>% enframe(name = 'nome', value = 'cluster_id'))
  )

# analisando melhor numero de clusters --------------------------------------------------------------------------------------------------------------------

## todos os valores brutos facetados
pam_k_values %>% 
  select(K, contains('sil_')) %>% 
  pivot_longer(cols = -K, names_to = 'dataset', values_to = 'silhueta') %>% 
  ggplot(mapping = aes(x = K, y = silhueta, group = dataset)) +
  facet_wrap(~ dataset, scales = 'free_y') +
  geom_line(mapping = aes(color = dataset)) +
  geom_point(mapping = aes(fill = dataset), color = 'black', shape = 21, size = 3) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

## todos os valores brutos no mesmo plot
pam_k_values %>% 
  select(K, contains('sil_')) %>% 
  pivot_longer(cols = -K, names_to = 'dataset', values_to = 'silhueta') %>% 
  ggplot(mapping = aes(x = K, y = silhueta, group = dataset)) +
  geom_line(mapping = aes(color = dataset)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

## quanto maior o número de clusteres, maior o número de singletons
pam_k_values %>% 
  select(K, contains('clustering')) %>% 
  pivot_longer(cols = -K, names_to = 'dataset', values_to = 'cluster') %>% 
  unnest(cols = cluster) %>% 
  arrange(K, dataset, cluster_id) %>% 
  count(K, dataset, cluster_id, name = 'titulos') %>% 
  group_by(K, dataset) %>% 
  summarise(singleton = sum(titulos == 1),
            doubleton = sum(titulos == 2)) %>% 
  mutate(total = singleton + doubleton) %>% 
  ungroup %>% 
  pivot_longer(cols = -c(K, dataset), names_to = 'natureza', values_to = 'ocorrencias') %>% 
  ggplot(mapping = aes(x = K, y = ocorrencias)) +
  facet_wrap(~ dataset) +
  geom_line(mapping = aes(color = natureza), size = 1) +
  scale_color_manual(values = c('firebrick', 'forestgreen', 'dodgerblue3'))

## identificando o último valor de K onde não temos singletons
pam_k_values %>% 
  select(K, contains('clustering')) %>% 
  pivot_longer(cols = -K, names_to = 'dataset', values_to = 'cluster') %>% 
  unnest(cols = cluster) %>% 
  arrange(K, dataset, cluster_id) %>% 
  count(K, dataset, cluster_id, name = 'titulos') %>% 
  group_by(K, dataset) %>% 
  summarise(singleton = sum(titulos == 1)) %>% 
  arrange(dataset, K) %>% 
  group_by(dataset) %>% 
  filter(singleton == 0) %>% 
  filter(K == max(K))

## focando nos valores de K que não contém singletons nos dois datasets-alvo
pam_k_values %>% 
  select(K, contains('sil_')) %>% 
  pivot_longer(cols = -K, names_to = 'dataset', values_to = 'silhueta') %>% 
  filter(K <= 39, dataset %in% c('sil_subset', 'sil_mec_tema')) %>% 
  ggplot(mapping = aes(x = K, y = silhueta, group = dataset)) +
  facet_wrap(~ dataset, scales = 'free_y') +
  geom_line(mapping = aes(color = dataset)) +
  geom_point(mapping = aes(fill = dataset), color = 'black', shape = 21, size = 3) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

## focando no dataset com o subset dos dados e 38 clusteres
pam_k_values %>% 
  select(K, contains('sil_')) %>% 
  pivot_longer(cols = -K, names_to = 'dataset', values_to = 'silhueta') %>% 
  filter(K <= 39, K > 2, dataset %in% c('sil_subset', 'sil_mec_tema')) %>% 
  group_by(dataset) %>% 
  top_n(n = 1, wt = silhueta)

# TSNE ----------------------------------------------------------------------------------------------------------------------------------------------------

# rodando o tsne com perplexidade de 38 - numero de clusteres
tsne_obj <- Rtsne(X = gower_subset, perplexity = 30, is_distance = TRUE, pca_center = FALSE, pca_scale = FALSE, normalize = FALSE)

# consolidando os resultados do TSNE
resultados_tsne <- dados %>% 
  select(nome) %>% 
  cbind(tsne_obj$Y %>% 
          data.frame() %>%
          setNames(c('X', 'Y'))) %>% 
  as_tibble() %>% 
  left_join(y = originais %>% 
              select(nome, complexidade, mecanicas, temas),
            by = 'nome'
  )

# visualização basica do tsne
resultados_tsne %>% 
  plot_ly() %>% 
  add_markers(x = ~ X, y = ~ Y, hoverinfo = 'text', color = I('dodgerblue3'), stroke = I('black'), size = I(20),
              text = ~ paste('<b>Título:</b>', nome, '<br>',
                             '<b>Mecânicas:</b>', mecanicas, '<br>',
                             '<b>Temas:</b>', temas
                             )
              ) %>% 
  layout(xaxis = list(title = '', showline = FALSE, showticklabels = FALSE, gridcolor = toRGB('white'), zerolinecolor = toRGB('grey60')), 
         yaxis = list(title = '', showline = FALSE, showticklabels = FALSE, gridcolor = toRGB('white'), zerolinecolor = toRGB('grey60')),
         hoverlabel = list(align = 'right')
  )

# PCOA  ---------------------------------------------------------------------------------------------------------------------------------------------------

# ordenacao atraves da PCoA
resultados_pcoa <- pcoa(D = gower_subset)

# eixos com maior explicabilidade
resultados_pcoa$values %>% 
  mutate(total = sum(Eigenvalues),
         relativo = cumsum(Eigenvalues / total)) %>% 
  filter(Eigenvalues > Broken_stick, relativo <=0.95)

# visualização da PCoA
resultados_pcoa$vectors %>% 
  data.frame %>% 
  rownames_to_column(var = 'nome') %>% 
  ggplot(mapping = aes(x = Axis.1, y = Axis.2)) +
  geom_point()

# juntando informações ------------------------------------------------------------------------------------------------------------------------------------

# medoids
jogos_medoids <- pam_k_values %>% 
  filter(K == 38) %>% 
  select(K, contains('_subset')) %>% 
  pull(med_subset) %>% 
  pluck(1)

# resultados da seleção
resultados <- pam_k_values %>% 
  filter(K == 28) %>% 
  select(K, contains('_subset')) %>% 
  pull(clustering_subset) %>% 
  pluck(1) %>% 
  left_join(y = resultados_tsne,
            by = 'nome') %>% 
  left_join(y = resultados_pcoa$vectors %>% 
              data.frame %>% 
              rownames_to_column(var = 'nome') %>% 
              select(nome, Axis.1, Axis.2),
            by = 'nome') %>% 
  mutate(medoid = nome %in% jogos_medoids)

# plot do tsne
resultados %>% 
  ggplot(mapping = aes(x = X, y = Y, fill = cluster_id)) +
  geom_point(shape = 21, size = 3, show.legend = FALSE) +
  scale_fill_viridis_c()

# plot da pcoa
resultados %>% 
  ggplot(mapping = aes(x = Axis.1, y = Axis.2, fill = cluster_id)) +
  geom_point(shape = 21, size = 3, show.legend = FALSE) +
  scale_fill_viridis_c()

# HIERARCHICAL CLUSTER ------------------------------------------------------------------------------------------------------------------------------------

# ajustando o cluster
hc <- hclust(d = gower_subset, method = 'ward.D2')

# VISUALIZANDO O CLUSTER ----------------------------------------------------------------------------------------------------------------------------------

# dendrograma padrao
plot(as.dendrogram(hc), horiz = TRUE)

# plotando o dendrograma cortado
plot(x = cut(as.dendrogram(hc), h = 0.2)$upper, horiz = TRUE)

# PODANDO O CLUSTER ---------------------------------------------------------------------------------------------------------------------------------------

# cortando o dendrograma na dissimilaridade de 0.2 com o cutree
cutree(tree = hc, h = 0.2)

# output do corte do dendrograma com o cut
cut(as.dendrogram(hc), h = 0.2)

# DEFININDO O IMPACTO DO VALOR DE H NO HIERARCHICAL CLUSTER -----------------------------------------------------------------------------------------------

## criando grid de valores de h
hc_h_values <- tibble(
  h_value = seq(from = 0.01, to = 0.99, by = 0.01)
)

# calculando a quantidade de clusteres por ponto de corte
hc_h_values <- hc_h_values %>% 
  mutate(cortes = map(.x = h_value, ~ cutree(tree = hc, h = .x)),
         n_cluster = map_dbl(.x = cortes, ~ length(unique(.x)))
  )

# plotando os valores de hc para cada valor de h
hc_h_values %>% 
  ggplot(mapping = aes(x = h_value, y = n_cluster)) +
  geom_line() +
  scale_y_continuous()

# GERANDO VISUALIZACAO FINAL ------------------------------------------------------------------------------------------------------------------------------

## quantidade de clusteres por ponto de corte
hc_h_values %>% 
  filter(h_value >= 0.15, h_value <= 0.25)

# setando o valor de corte
h_value <- 0.20

# cortando o dendrograma e colocando ele dentro do objeto do ggdendro
ddh <- dendro_data(model = cut(as.dendrogram(hc), h = h_value)$upper, type = "rectangle")

# extraindo os clusteres de cada jogo 
cluster_jogo <- cutree(tree = hc, h = h_value) %>% 
  enframe(name = 'nome', value = 'cluster_id')

# criando os dados para o segmento
segment_data <- segment(ddh) %>% 
  mutate(yend = ifelse(test = yend < h_value, yes = h_value, no = yend)) 

# criados os dados para os pontos
point_data <- ddh %>% 
  pluck('labels') %>% 
  mutate(cluster_id = str_extract(string = label, pattern = '[0-9]+'),
         label = str_replace(string = label, pattern = 'Branch\\s', replacement = 'Grupo '),
         cluster_id = as.character(cluster_id)
  ) %>% 
  left_join(y = cluster_jogo %>% 
              group_by(cluster_id) %>% 
              summarise(titulos = paste0(sort(nome), collapse = ', '),
                        quantidade = n()) %>% 
              mutate(cluster_id = as.character(cluster_id)),
            by = 'cluster_id'
  ) %>% 
  left_join(y = cluster_jogo %>% 
              group_by(cluster_id) %>%
              mutate(ranking_within_cluster = row_number()) %>% 
              filter(ranking_within_cluster <= 3) %>% 
              summarise(exemplos = paste0(sort(nome), collapse = ', ')) %>% 
              mutate(cluster_id = as.character(cluster_id)),
            by = 'cluster_id'
  ) %>% 
  cbind(segment_data %>% 
          filter(yend == 0.20) %>% 
          select(x_fixed = x)) %>% 
  as_tibble %>% 
  mutate(y = 0.19)

# criando a figura
plot_ly() %>% 
  add_segments(data = segment_data, x = ~ y, y = ~ x, xend = ~yend, yend = ~ xend, color = I('black'), showlegend = FALSE#,
               #hoverinfo = 'text', text = ~ paste('<b>Similaridade no Ramo:</b>', round(x = yend, digits = 4))
  ) %>% 
  add_markers(data = point_data, x = ~y, y = ~x_fixed, color = I('dodgerblue3'), stroke = I('black'), size = I(20),
              hoverinfo = 'text', text = ~ paste('<b>Grupo:</b>', cluster_id, '<br>',
                                                 '<b>Qtd. de Títulos:</b>', quantidade, '<br>',
                                                 '<b>Exemplos:</b>', exemplos)
  ) %>% 
  layout(xaxis = list(title = '<b>Dissimilaridade</b>', zeroline = FALSE, showline = FALSE, showgrid = FALSE, autorange = 'reversed'), 
         yaxis = list(title = '', zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE)
  )
