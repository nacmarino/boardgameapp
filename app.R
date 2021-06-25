
# limpando o ambiente -------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

# carregando pacotes --------------------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(gower)
library(plotly)
library(ggdendro)
library(tidyverse)
library(reactable)
library(shinyWidgets)
library(shinythemes)

# carregando os dados -------------------------------------------------------------------------------------------------------------------------------------

## carregando os dados brutos
dados <- read_rds(file = 'data/dados.rds') 

## carregando os dicionarios de temas, categorias e mecanicas
temas <- read_rds(file = 'data/temas.rds')
mecanicas <- read_rds(file = 'data/mecanicas.rds')
categorias <- read_rds(file = 'data/categorias.rds')

## carregando a matriz de categorias, temas e mecanicas
matriz <- read_rds(file = 'data/matriz.rds')

## passando a matriz para um dataframe
matriz_df <- data.frame(matriz) %>% 
  `rownames<-`(value = .$nome) %>% 
  select(-nome)

## DADOS PARA A ABA DOS DENDROGRAMAS
resultados_tsne <- read_rds(file = 'data/resultados_tsne.rds')
hc_all <- read_rds(file = 'data/dendrograma_hc_all.rds')
hc_h_values <- read_rds(file = 'data/hc_h_values.rds')
hc_cutted <- read_rds(file = 'data/dendrograma_cortado.rds')
hc_grupos <- read_rds(file = 'data/dendrograma_subgrupos.rds')

# criando app ---------------------------------------------------------------------------------------------------------------------------------------------
# ui ------------------------------------------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(
  title = 'BoardGameApp',
  theme = shinytheme(theme = 'flatly'),
  tabPanel(
    title = 'Home',
    icon = icon(name = 'home'),
    fluidRow(
      column(width = 2
      ),
      column(width = 8,
             includeMarkdown(path = 'markdown/home.Rmd')
      ),
      column(width = 2
      )
    )
  ),
  tabPanel(
    title = 'Busca',
    icon = icon(name = 'search'),
    sidebarLayout(
      sidebarPanel = sidebarPanel(width = 2,
                                  h4('Quais as características do jogo que você está buscando?'),
                                  br(),
                                  selectInput(inputId = 'publico_input', 
                                              label = 'Qual o público do jogo?',
                                              choices = c('Infantil', 'Familiar', 'Expert', 'Indeterminado'), 
                                              multiple = TRUE, 
                                              selectize = TRUE
                                  ),
                                  br(),
                                  selectInput(inputId = 'complexidade_input', 
                                              label = 'Qual a complexidade das regras?',
                                              choices = c('Nenhuma', 'Baixa', 'Moderada', 'Alta', 'Indeterminado'), 
                                              multiple = TRUE, 
                                              selectize = TRUE
                                  ),
                                  br(),
                                  selectInput(inputId = 'tema_input', 
                                              label = 'Busca algum tema específico?',
                                              choices = c('Não', sort(unique(temas$temas))), 
                                              selected = 'Não'
                                  ),
                                  br(),
                                  selectInput(inputId = 'mecanica_input', 
                                              label = 'Busca alguma mecânica específica?',
                                              choices = c('Não', sort(unique(mecanicas$mecanicas))), 
                                              selected = 'Não'
                                  ),
                                  br(),
                                  sliderTextInput(inputId = 'jogadores_input', 
                                                  label = 'Para quantos jogadores?',
                                                  choices = sort(unique(c(dados$n_jogadores_min, dados$n_jogadores_max))),
                                                  selected = c(min(dados$n_jogadores_min), max(dados$n_jogadores_max)), 
                                                  grid = TRUE, 
                                                  force_edges = TRUE
                                  ),
                                  br(),
                                  sliderTextInput(inputId = 'duracao_input', 
                                                  label = 'Qual a duração da partida?',
                                                  choices = sort(unique(c(dados$duracao_partida, dados$duracao_partida))),
                                                  selected = c(min(dados$duracao_partida), max(dados$duracao_partida)),
                                                  post = ' min',
                                                  grid = TRUE,
                                                  force_edges = TRUE
                                  ),
                                  br(),
                                  sliderInput(inputId = 'idade_input',
                                              label = 'A partir de qual idade?', 
                                              min = min(dados$idade_minima),
                                              max = max(dados$idade_minima),
                                              value = min(dados$idade_minima), 
                                              step = 1, 
                                              post = '+'),
                                  br(),
                                  checkboxInput(inputId = 'disponiveis_input', 
                                                label = 'Me mostre apenas os jogos disponíveis no Brasil.', 
                                                value = FALSE)
      ), 
      mainPanel = mainPanel(width = 10,
                            fluidRow(
                              column(width = 12,
                                     h1('Busca pelos jogos no Ranking'),
                                     br(),
                                     p('A tabela abaixo sumariza as informações dos 500 jogos de tabuleiro mais bem posicionados no ranking da Ludopedia.')
                              )
                            ),
                            fluidRow(
                              column(width = 12,
                                     reactableOutput(outputId = 'jogos_selecionados_output')
                              )
                            )
      )
    )
  ),
  tabPanel(
    title = 'Deu match!',
    icon = icon(name = 'check'),
    sidebarLayout(sidebarPanel = sidebarPanel(width = 3,
                                              h4('Quer uma recomendação baseada em algum jogo que você conhece?'),
                                              br(),
                                              selectInput(inputId = 'jogo_input', 
                                                          label = 'Gostaria de jogos parecidos com...',
                                                          choices = sort(unique(dados$nome)),
                                                          selected = ''),
                                              br(),
                                              sliderInput(inputId = 'n_similar_input',
                                                          label = 'Quantos jogos similares a esse devem ser sugeridos?', 
                                                          min = 1, 
                                                          max = 20, 
                                                          value = 1, 
                                                          step = 1),
                                              br(),
                                              fluidRow(
                                                column(width = 12,
                                                       checkboxGroupInput(inputId = 'subset_match_input', 
                                                                          label = 'Gostaria que os jogos fossem parecidos quanto à/ao...',
                                                                          choices = c('Mecânica', 'Tema', 'Categoria'),
                                                                          selected = c('Mecânica', 'Tema', 'Categoria'),
                                                                          inline = TRUE
                                                       )
                                                )
                                              ),
                                              br(),
                                              actionButton(inputId = 'gera_similar_input', 
                                                           label = 'Gere a sugestão!',
                                                           icon = icon(name = 'cogs'))
    ),
    mainPanel = mainPanel(
      width = 9,
      fluidRow(
        column(width = 12,
               h1('Quais os jogos mais parecidos com aquele que busco?'),
               br(),
               p('Utilize o menu ao lado para selecionar um jogo específico e a quantidade de jogos similares a ele que você deseja buscar. Quando estiver pronto, basta clicar no botão e as sugestões serão geradas.'),
               br(),
               fluidRow(
                 column(width = 12,
                        h3(textOutput(outputId = 'nome_jogo_output'))
                 )
               ),
               fluidRow(
                 column(width = 2,
                        uiOutput('img_jogo_selecionado_output')
                 ),
                 column(width = 10,
                        h5(textOutput(outputId = 'descricao_jogo_match_output'))
                 )
               ),
               br(),
               h4(textOutput(outputId = 'jogo_selecionado_output')),
               br(),
               fluidRow(
                 column(width = 12,
                        reactableOutput(outputId = "tabela_match_output")
                 )
               )
        )
      )
    )
    )
  ),
  navbarMenu(title = 'Queria saber...',
             icon = icon(name = 'question'),
             tabPanel(
               title = 'Como anda a publicação dos jogos?',
               icon = icon(name = 'hourglass'),
               fluidRow(
                 column(width = 2
                 ),
                 column(width = 8,
                        includeHTML(path = 'www/analise_temporal.html')
                 ),
                 column(width = 2
                 )
               )
             ),
             tabPanel(
               title = 'Quantos tipos diferentes de jogos existem?',
               icon = icon(name = 'object-ungroup'),
               fluidRow(
                 column(width = 2
                 ),
                 column(width = 8,
                        fluidRow(column(width = 12,
                                        includeHTML(path = 'www/analise_cluster_parte_1.html')
                        )
                        ),
                        fluidRow(column(width = 12,
                                        div(plotlyOutput('cluster_tsne_output', width = '800px', height = '800px'), align = 'center')
                        )
                        ),
                        fluidRow(column(width = 12,
                                        includeMarkdown(path = 'markdown/analise_cluster_parte_2.Rmd')
                        )
                        ),
                        fluidRow(column(width = 12,
                                        div(plotlyOutput('cluster_dendrograma_output', height = '1000px'), align = 'center')
                        )
                        ),
                        fluidRow(column(width = 12,
                                        includeMarkdown(path = 'markdown/analise_cluster_parte_3.Rmd')
                        )
                        ),
                        br(),
                        fluidRow(column(width = 12,
                                        div(plotlyOutput('cluster_ponto_de_corte_output', width = '500px', height = '500px'), align = 'center')
                        )
                        ),
                        br(),
                        fluidRow(column(width = 12,
                                        includeMarkdown(path = 'markdown/analise_cluster_parte_4.Rmd')
                        )
                        ),
                        br(),
                        fluidRow(column(width = 12,
                                        div(plotlyOutput('cluster_dendrograma_final_output', height = '800px'), align = 'center')
                        )
                        ),
                        br(),
                        fluidRow(column(width = 12,
                                        includeHTML(path = 'www/analise_cluster_parte_5.html')
                        )
                        ),
                        br(),
                        fluidRow(column(width = 6,
                                        selectInput(inputId = 'input_dendrograma_grupos', 
                                                    label = 'Selecione o grupo desejado', 
                                                    choices = unique(hc_grupos$grupo),
                                                    selected = 'Grupo 1', 
                                                    multiple = FALSE
                                        )
                        )
                        ),
                        fluidRow(column(width = 12,
                                        div(plotlyOutput('cluster_dendrograma_grupos_output'), align = 'center')
                        )
                        ),
                        fluidRow(column(width = 12,
                                        includeMarkdown(path = 'markdown/analise_cluster_parte_6.Rmd')
                        )
                        )
                 ),
                 column(width = 2
                 )
               )
             ),
             tabPanel(
               title = 'Quem são as estrelas?',
               icon = icon(name = 'star'),
               fluidRow(
                 column(width = 2
                 ),
                 column(width = 8,
                        includeMarkdown(path = 'markdown/analise_estrelas.Rmd')
                 ),
                 column(width = 2
                 )
               )
             )
  ),
  tabPanel(
    title = 'Sobre',
    icon = icon(name = 'info'),
    fluidRow(
      column(width = 2
      ),
      column(width = 8,
             includeMarkdown(path = 'markdown/sobre.Rmd')
      ),
      column(width = 2
      )
    )
  )
)

# servidor ------------------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # TABPANEL_BUSCA -------------------------------------------------------
  
  # TABPANEL_BUSCA_REACTIVE ----------------------------------------------
  tabela_jogos <- reactive({
    # copiando os dados originais
    tabela_jogo <- dados
    
    ## FILTRO DE PUBLICO -------------------------------------------------
    # pegando o string do filtro de publico
    search_publico <- input$publico_input
    # implementando o filtro de publico
    if(!'' %in% search_publico) {
      # criando o string de busca
      if(length(search_publico) == 1) {
        search_string <- search_publico
      } else {
        search_string <- paste0(search_publico, collapse = '|')
      }
      # filtrando a coluna de publico
      tabela_jogo <- filter(tabela_jogo, str_detect(string = publico, pattern = search_string))
    }
    
    ## FILTRO DE COMPLEXIDADE --------------------------------------------
    # pegando o string do filtro de complexidade
    search_complexidade <- input$complexidade_input
    # implementando o filtro de publico
    if(!'' %in% search_complexidade) {
      # criando o string de busca
      if(length(search_complexidade) == 1) {
        search_string <- search_complexidade
      } else {
        search_string <- paste0(search_complexidade, collapse = '|')
      }
      # filtrando a coluna de publico
      tabela_jogo <- filter(tabela_jogo, str_detect(string = complexidade, pattern = search_string))
    }
    
    ## FILTRO DE TEMA ----------------------------------------------------
    ## aplicando filtro apenas se a escolha for diferente de não
    if(input$tema_input != 'Não'){
      tabela_jogo <- filter(tabela_jogo, str_detect(string = temas, pattern = input$tema_input))
    }
    
    ## FILTRO DE MECANICA ------------------------------------------------
    ## aplicando filtro apenas se a escolha for diferente de não
    if(input$mecanica_input != 'Não'){
      tabela_jogo <- filter(tabela_jogo, str_detect(string = mecanicas, pattern = input$mecanica_input))
    }
    
    ## FILTRO DE QTD DE JOGADORES ----------------------------------------
    
    ## filtrando o mínimo e máximo de jogadores permitidos
    tabela_jogo <- filter(tabela_jogo, n_jogadores_min >= min(input$jogadores_input) & n_jogadores_max <= max(input$jogadores_input))
    
    ## FILTRO DE DURACAO -------------------------------------------------
    ## filtrando o mínimo e máximo da duração do jogo
    tabela_jogo <- filter(tabela_jogo, duracao_partida >= min(input$duracao_input) & duracao_partida <= max(input$duracao_input))
    
    ## FILTRO DE IDADE ---------------------------------------------------
    ## filtrando a idade minima recomendada
    tabela_jogo <- filter(tabela_jogo, idade_minima >= input$idade_input)
    
    ## FILTRO DE DISPONIBILIDADE -----------------------------------------
    ## filtrando apenas os jogos que estão disponíveis no brasil se for o caso
    if(input$disponiveis_input){
      tabela_jogo <- filter(tabela_jogo, disponivel_br == 'Sim')
    }
    
    ## SELECIONANDO COLUNAS PARA EXIBIÇÃO --------------------------------
    # selecionando as colunas
    tabela_jogo <- select(tabela_jogo, nome, nota_geral, n_notas, ano_criacao, complexidade, n_jogadores, duracao_partida,
                          mecanicas, temas, editora_brasileira)
    
  })
  
  
  # TABPANEL_BUSCA_OUTPUTS -----------------------------------------------
  output$jogos_selecionados_output <- renderReactable(expr = {
    
    reactable(data = tabela_jogos(),
              defaultColDef = colDef(align = 'center', headerStyle = list(background = '#f7f7f8')),
              columns = list(
                nome = colDef(name = 'Título', maxWidth = 125, style = function() {list(fontWeight = 'bold')}),
                ano_criacao = colDef(name = 'Ano', maxWidth = 70),
                nota_geral = colDef(name = 'Nota', maxWidth = 70),
                n_notas = colDef(name = 'Votos', maxWidth = 70),
                complexidade = colDef(name = 'Complexidade', maxWidth = 120),
                n_jogadores = colDef(name = 'Jogadores', maxWidth = 90),
                duracao_partida = colDef(name = 'Duração', format = colFormat(suffix = ' min'), maxWidth = 100),
                mecanicas = colDef(name = 'Mecânicas', minWidth = 200),
                temas = colDef(name = 'Temas', minWidth = 150),
                editora_brasileira = colDef(name = 'Editora', maxWidth = 100)
              ), 
              defaultPageSize = 8,
              highlight = TRUE,
              height = 800, 
              searchable = TRUE, 
              striped = TRUE, 
              compact = TRUE, 
              paginationType = 'jump'
    )
    
  })
  
  # TABPANEL_MATCH -------------------------------------------------------
  
  # TABPANEL_MATCH_REACTIVE ----------------------------------------------
  
  ## pegando o nome do jogo do input somente quando o botão é clicado
  nome_jogo <- eventReactive(eventExpr = input$gera_similar_input, 
                             valueExpr = {
                               input$jogo_input
                             }
  )
  
  ## pegando a quantidade de matches do input somente quando o botão é clicado
  n_matches <- eventReactive(eventExpr = input$gera_similar_input, 
                             valueExpr = {
                               input$n_similar_input
                             }
  )
  
  ## pegando a descrição do jogo selecionado somente quando o botão é clicado
  descricao_jogo_selecionado <- eventReactive(eventExpr = input$gera_similar_input, 
                                              valueExpr = {
                                                filter(dados, nome == input$jogo_input) %>% 
                                                  pull(descricao)
                                              }
  )
  
  ## pegando a url da imagem da caixa do jogo somente quando o botão é clicado
  url_imagem <- eventReactive(eventExpr = input$gera_similar_input, 
                              valueExpr = {
                                filter(dados, nome == input$jogo_input) %>% 
                                  pull(image_link)
                              }
  )
  
  ## criando a matriz que será usada para achar os matches de acordo com o subset de colunas
  matriz_match <- eventReactive(eventExpr = input$gera_similar_input,
                                valueExpr = {
                                  ## copiando o dataframe
                                  matriz_df_match <- matriz_df
                                  ## removendo as colunas de mecanica
                                  if(!'Mecânica' %in% input$subset_match_input){
                                    matriz_df_match <- select(matriz_df_match, -contains('mecanica'))
                                  }
                                  ## removendo as colunas de tema
                                  if(!'Tema' %in% input$subset_match_input){
                                    matriz_df_match <- select(matriz_df_match, -contains('tema'))
                                  }
                                  ## removendo as colunas de categoria
                                  if(!'Categoria' %in% input$subset_match_input){
                                    matriz_df_match <- select(matriz_df_match, -contains('categoria'))
                                  } 
                                  ## retornando a matriz final
                                  matriz_df_match
                                  
                                }
  )
  
  ## pegando as informações do jogo desejado
  jogo_selecionado <- eventReactive(eventExpr = input$gera_similar_input,
                                    valueExpr = {
                                      ## pegando o indice do jogo selecionado na matriz de características
                                      indice_jogo <- which(rownames(matriz_match()) == nome_jogo())
                                      
                                      ## criando um subset da matriz com o jogo selecionado
                                      matriz_match()[indice_jogo, ]
                                    }
  )
  
  ## pegando as informações dos outros jogos
  outros_jogos <- eventReactive(eventExpr = input$gera_similar_input,
                                valueExpr = {
                                  ## pegando o indice do jogo selecionado na matriz de características
                                  indice_jogo <- which(rownames(matriz_match()) == nome_jogo())
                                  
                                  ## criando um subset da matriz com o jogo selecionado
                                  matriz_match()[-indice_jogo, ]
                                }
  )
  
  ## calculando os matches e trazendo os jogos
  deu_match <- eventReactive(eventExpr = input$gera_similar_input, 
                             valueExpr = {
                               ## calculando os melhores matches
                               melhores_matches <- gower_topn(x = jogo_selecionado(), y = outros_jogos(), n = n_matches())
                               
                               ## extraindo o nome dos jogos que deram match
                               resultado_dos_matches <- data.frame(nome = rownames(outros_jogos()[melhores_matches$index,]), 
                                                                   distancia = melhores_matches$distance)
                               
                               left_join(x = resultado_dos_matches, y = dados, by = 'nome') %>% 
                                 mutate(ordem = row_number()) %>% 
                                 select(ordem, nome, descricao, disponivel_br) 
                             }
  )
  
  # TABPANEL_MATCH_OUTPUTS -----------------------------------------------
  
  ## nome do jogo selecionado
  output$nome_jogo_output <- renderText(expr = {
    nome_jogo()
  }
  )
  
  ## foto da caixa do jogo
  output$img_jogo_selecionado_output <- renderUI(expr = {
    ## passando a url da imagem
    tags$img(src = url_imagem())
  })
  
  ## descrição do jogo selecionado
  output$descricao_jogo_match_output <- renderText(expr = {
    descricao_jogo_selecionado()
  }
  )
  
  ## gerando o output após o clique do botão
  output$jogo_selecionado_output <- renderText(expr = {
    if(n_matches() == 1){
      str_glue('Este é o jogo de tabuleiro mais similar à {nome_jogo()}.')
    } else {
      str_glue('Estes são os {n_matches()} jogos de tabuleiro mais similares à {nome_jogo()}.')
    }
  })
  
  ## gerando a tabela dos matches que será exibida
  output$tabela_match_output <- renderReactable(expr = {
    
    reactable(data = deu_match(), 
              defaultColDef = colDef(align = 'center', headerStyle = list(background = '#f7f7f8')),
              columns = list(
                ordem = colDef(name = 'Ordem', maxWidth = 80, format = colFormat(suffix = 'º'), style = function() {list(fontWeight = 'bold')}),
                nome = colDef(name = 'Título', maxWidth = 125, style = function() {list(fontWeight = 'bold')}),
                descricao = colDef(name = 'Descrição do Jogo'),
                disponivel_br = colDef(name = 'Disponível no Brasil?', maxWidth = 90)
              ),
              highlight = TRUE,
              striped = TRUE,
              height = 700,
              defaultPageSize = 3
    )
  }
  )
  
  # TABPANEL_ANALISE_CLUSTER ---------------------------------------------
  # TABPANEL_ANALISE_CLUSTER_FIGURAS -------------------------------------
  
  # figura da ordenação do TSNE
  output$cluster_tsne_output <- renderPlotly(expr = {
    # criando a figura
    resultados_tsne %>% 
      plot_ly() %>% 
      add_markers(x = ~ X, y = ~ Y, hoverinfo = 'text', color = I('dodgerblue3'), stroke = I('black'), size = I(20),
                  text = ~ paste('<b>Título:</b>', nome, '<br>',
                                 '<b>Mecânicas:</b>', mecanicas, '<br>',
                                 '<b>Temas:</b>', temas
                  )
      ) %>% 
      layout(title = '<b>Relação dos Títulos de acordo com sua Dissimilaridade</b>',
             xaxis = list(title = '', showline = FALSE, showticklabels = FALSE, gridcolor = toRGB('white'), zerolinecolor = toRGB('grey60')), 
             yaxis = list(title = '', showline = FALSE, showticklabels = FALSE, gridcolor = toRGB('white'), zerolinecolor = toRGB('grey60')),
             hoverlabel = list(align = 'right')
      )
  })
  
  # figura do dendrograma completo
  output$cluster_dendrograma_output <- renderPlotly(expr = {
    # criando a figura
    plot_ly() %>% 
      add_segments(data = segment(hc_all), x = ~ y, y = ~ x, xend = ~yend, yend = ~ xend, color = I('black'), showlegend = FALSE,
                   hoverinfo = 'text', text = ~ paste('<b>Dissimilaridade no Ramo:</b>', round(x = yend, digits = 4))
      ) %>% 
      add_markers(data = label(hc_all), x = ~y, y = ~x, color = I('black'), stroke = I('black'), size = I(5),
                  hoverinfo = 'text', text = ~ paste('<b>Título:</b>', label)
      ) %>% 
      layout(title = '<b>Padrão de agrupamento dos títulos</b>',
             xaxis = list(title = '<b>Dissimilaridade</b>', zeroline = FALSE, showline = FALSE, showgrid = FALSE, autorange = 'reversed'), 
             yaxis = list(title = '', zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE),
             hoverlabel = list(align = 'right')
      )
  })
  
  # figura da relacao do ponto de corte e quantidade de clusters
  output$cluster_ponto_de_corte_output <- renderPlotly(expr = {
    # criando a figura
    plot_ly() %>% 
      add_trace(data = hc_h_values, x = ~ h_value, y = ~ n_cluster, color = I('black'), mode = 'lines+markers',
                hoverinfo = 'text', text = ~ paste('<b>Ponto de Corte:</b>', h_value, '<br>',
                                                   '<b>Qtd. de Grupos:</b>', n_cluster)
      ) %>% 
      layout(title = '<b>Ponto de corte e quantidade de grupos</b>',
             xaxis = list(title = '<b>Ponto de Corte da Dissimilaridade</b>'), 
             yaxis = list(title = '<b>Quantidade de Grupos</b>'),
             hoverlabel = list(align = 'right')
      )
  })
  
  # figura do dendrograma com o ponto de corte
  output$cluster_dendrograma_final_output <- renderPlotly(expr = {
    # criando a figura
    plot_ly() %>% 
      add_segments(data = hc_cutted$segmentos, x = ~ y, y = ~ x, xend = ~yend, yend = ~ xend, color = I('black'), showlegend = FALSE,
                   hoverinfo = 'text', text = ~ paste('<b>Similaridade no Ramo:</b>', round(x = yend, digits = 4))
      ) %>% 
      add_text(data = hc_cutted$pontos, x = ~y, y = ~x_fixed, text = ~ label, textposition = 'right',
               hovertemplate = ~ paste('<b>Grupo:</b>', cluster_id, '<br>',
                                       '<b>Qtd. de Títulos:</b>', quantidade, '<br>',
                                       '<b>Exemplos:</b>', exemplos)
      ) %>% 
      layout(title = '<b>Padrão de agrupamento dos títulos</b>',
             xaxis = list(title = '<b>Dissimilaridade</b>', zeroline = FALSE, showline = FALSE, showgrid = FALSE, range = c(1, -0.2), 
                          tickvals = seq(from = 1.0, to = 0.2, by = -0.2)), 
             yaxis = list(title = '', zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE),
             hoverlabel = list(align = 'right')
      )
  })
  
  # figura do dendrograma dos subgrupos
  output$cluster_dendrograma_grupos_output <- renderPlotly(expr = {
    
    # pegando o dendrograma selecionado
    dendro_selecionado <- hc_grupos %>% 
      filter(grupo == input$input_dendrograma_grupos) %>% 
      #filter(grupo == 'Grupo 1') %>% 
      pull(dendrograma) %>% 
      pluck(1) %>% 
      dendro_data(model = .)
    
    # pegando informações dos jogos no dendrograma
    dendro_selecionado_jogos <- dendro_selecionado$labels %>% 
      as_tibble() %>% 
      left_join(y = dados %>% 
                  select(nome, complexidade, mecanicas, temas),
                by = c('label' = 'nome')) %>% 
      mutate(y = -0.01)
    
    # plotando a figura
    plot_ly() %>% 
      add_segments(data = segment(dendro_selecionado), x = ~ y, y = ~ x, xend = ~yend, yend = ~ xend, color = I('black'), showlegend = FALSE) %>% 
      add_text(data = dendro_selecionado_jogos, x = ~y, y = ~x, text = ~ label, textposition = 'right',
               hovertemplate = ~ paste('<b>Mecânicas:</b>', mecanicas, '<br>',
                                       '<b>Temas:</b>', temas)
      ) %>% 
      layout(xaxis = list(title = '<b>Dissimilaridade</b>', zeroline = FALSE, showline = FALSE, showgrid = FALSE, range = c(0.2, -0.2), 
                          tickvals = seq(from = 0.2, to = 0, by = -0.05)), 
             yaxis = list(title = '', zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE),
             hoverlabel = list(align = 'right')
      )
  })
  
}

# criando o app -------------------------------------------------------------------------------------------------------------------------------------------

shinyApp(ui, server)