library(shiny)
library(DT)
#library(plotly)
library(formattable)
library(readxl)
#library(plyr)
library(tidyr)
library(dplyr)
library(lazyeval)
library(ggplot2)
library(ggdendro)
library(ggthemes)
library(xlsx)
library(rJava)
library(xlsxjars)
library(rmarkdown)

shinyUI(
  
  tagList(tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar{background: #00a90a}")), # this is actually .css; this changes the color for the sliders
          navbarPage("App Inventário de Nativas",
                     
                     theme = "green_yeti2.css",
                     # theme = "green.css", # seleciona um tema contido na pasta www
                     # theme = shinythemes::shinytheme("paper"), # seleciona um tema utilizando pacote
                     
                     # Painel Intro ####          
                     tabPanel( "Intro" ,
                               fluidRow(
                                 column(5,
                                        includeMarkdown("about.md")
                                 ),
                                 column(6,
                                        img(contentType = "image/jpg",
                                            src="intro_picture.jpg",
                                            width = 770,
                                            #           height = 750)
                                            height = 856)
                                        
                                 )
                               ) # fluid row
                     ), # Painel Intro             
                     
                     
                     # Upload de dados ####
                     tabPanel("Importação",
                              sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  h3("Dados"),
                                  
                                  radioButtons("df_select", 
                                               "Fazer o upload de um arquivo, ou utilizar o dado de exemplo?", 
                                               c("Fazer o upload", "Utilizar o dado de exemplo"), 
                                               selected = "Fazer o upload"),
                                  
                                  uiOutput("upload"), # tipos de arquivos aceitos
                                  hr(),
                                  uiOutput("upload_csv"), # tipos de arquivos aceitos
                                  uiOutput("upload_xlsx") # tipos de arquivos aceitos
                                  
                                  
                                ), # sidebarPanel
                                
                                mainPanel(
                                  DT::dataTableOutput("rawdata")
                                ) # mainPanel
                              ) # sidebarLayout
                     ),
                     
                     # Mapeamento ####
                     tabPanel("Mapeamento de variáveis",
                     fluidPage(
                       
                       #h1("Shiny", span("Widgets Gallery", style = "font-weight: 300"), 
                       h1("Definição dos nomes das variáveis", 
                          style = "text-align: center;"),
                       br(),
                     #  h4("Nesta aba serão indicados os nomes das colunas que serão utilizadas nas análises em todo o app"),
                       fluidRow(
                         
                         column(4,
                                wellPanel(
                                  h3("Espécie"),
                                  p("Selecione o nome da variável referente à Espécie:"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                    ),
                                  uiOutput("selec_especies")
                                )), # Coluna Espécie
                       
                       column(4,
                              wellPanel(
                                h3("Parcela"),
                                p("Selecione o nome da variável referente à Parcela:"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_parcelas")
                              )),
                       
                       column(4,
                              wellPanel(
                                h3("Diâmetro (DAP)"),
                                p("Selecione o nome da variável referente à DAP:"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_dap")
                              )) # Coluna dap
                       
                       ), # fluidRow 1
                       
                       fluidRow(
                        
                         
                         column(4,
                                wellPanel(
                                  h3("Altura total"),
                                  p("Selecione o nome da variável referente à Altura total:"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_ht")
                                )), # Coluna ht
                         
                         column(4,
                                wellPanel(
                                  h3("Volume com casca"),
                                  p("Selecione o nome da variável referente à Volume com casca:"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_vcc")
                                )), # Coluna vcc
                         
                         column(4,
                                wellPanel(
                                  h3("Volume sem casca"),
                                  p("Selecione o nome da variável referente à Volume sem casca:"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_vsc")
                                )) # Coluna vsc
                         
                         
                         
                       ),# fluidRow 2
                       
                     fluidRow(
                       
                       column(4,
                              wellPanel(
                                h3("Área da parcela"),
                                p("Selecione o nome da variável referente à Área da parcela:"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_area.parcela")
                              )), # Coluna area.parcela
                       
                       
                       column(4,
                              wellPanel(
                                h3("Área total"),
                                p("Selecione o nome da variável referente à Área total"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_area.total")
                              )), # Coluna area.total
                       
                       
                       column(4,
                              wellPanel(
                                h3("Nível de agrupamento"),
                                p("Selecione o nome da variável referente ao nível de agrupamento"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_agrup")
                              )) # Coluna area.total
                       
                       
                     ), # fluidRow 3  
                       
                     
                     fluidRow(
                       
                       
                       column(4,
                              wellPanel(
                                h3("Estrutura vertical"),
                                p("Selecione o nome da variável referente à Estrutura vertical"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_est.vertical")
                              )), # Coluna area.total
                       
                       
                       column(4,
                              wellPanel(
                                h3("Estrutura interna"),
                                p("Selecione o nome da variável referente à Estrutura interna"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_est.interna")
                              )) # Coluna area.total
                       
                       
                     ) # fluidRow 4
                       
                     ) # fluidPage 
                     
                     
                     ),# tabPanel Mapeamento
                     
                     # tabPanel Preparação ####
                     tabPanel("Preparação dos dados", 
                              
                              
                              fluidPage(
                                
                                fluidRow(
                                  
                                  h1("Preparação dos dados",style = "text-align: center;"),
                                  br()
                                ),
                                
                                
                                fluidRow(
                                  
                                  sidebarPanel(
                                    
                                    h3("Intervalo de classe"),
                                    numericInput("int.classe", "Insira o intervalo de classe:", 10, 1, 50, 0.5),
                                    
                                    h3("Diâmetro mínimo"),
                                    numericInput("diam.min", "Insira o diâmetro mínimo:", 10, 1, 100, 1),
                                    
                                    uiOutput("selec_rotuloNI"),
                                    
                                    h3("Remover dados"),
                                    
                                    uiOutput("rm_data_var"),
                                    uiOutput("rm_data_level"),
                                    uiOutput("rm_vars"),
                                    uiOutput("selec_area_parcela_num"),
                                    uiOutput("selec_area_total_num"),
                                    uiOutput("ui_estvol1"),
                                    uiOutput("ui_estvol3"),
                                    uiOutput("ui_estvol4"),
                                    uiOutput("checkbox_calc.est.vert")
                                    
                                    
                                  ),# sidebarPanel
                                  
                                  mainPanel(p("colocar aviso sobre remover colunas importantes"),
                                            tableOutput("teste"),
                                            DT::dataTableOutput("prep_table")
                                  )# mainPanel
                                  
                                  
                                )
                                
                              )
                              
                              
                              
                              
                     ), # tabPanel filtrar dados
                     # navbarMenu fitossociologica ####
                     navbarMenu("Análise fitossociológica",
                                
                                # Diversidade ####
                                tabPanel("Índices de diversidade",
                                         
                                         fluidPage(
                                           h1("Índices de diversidade", style = "text-align: center;"),
                                           br(),
                                           fluidRow(
                                             column(5,
                                                    radioButtons("rb_div",
                                                                 h3("Calcular diversidade por parcela?"),
                                                                 c("Sim","Nao"),
                                                                 "Nao",
                                                                 TRUE),
                                                    offset = 7
                                           )),
                                           DT::dataTableOutput("div")
                                         )
                                         
                                ), #Diversidade tab
                                
                                # Similaridade ####
                                tabPanel("Índices de similaridade",
                                         
                                         fluidPage(
                                           
                                           h1("Índices de diversidade", style = "text-align: center;"),
                                           br(),
                                           
                                           fluidRow( 
                                             tabsetPanel(id = "mainPanel_Indices",
                                                         tabPanel("Matriz de Similaridadede de Jaccard", DT::dataTableOutput("msim1") ),
                                                         tabPanel("Dendrograma Jaccard", plotOutput("msim1_graph_",height = "600px"), value="id_msim1_graph" ),
                                                         tabPanel("Matriz de Similaridadede de Sorensen", DT::dataTableOutput("msim2") ),
                                                         tabPanel("Dendrograma Sorensen", plotOutput("msim2_graph_",height = "600px"), value="id_msim2_graph" ) 
                                                         
                                             ) ),
                                          fluidRow( column(width=4,uiOutput("rb_graphmsim"),offset = 3 ), 
                                                    column(width=3,uiOutput("slider_graphmsim")) )
                                             
                                           )
                                         
                                ), # Similaridade tab
                                
                                
                                # Agregação ####
                                tabPanel("Índices de agregação",
                                         
                                         fluidPage(
                                           h1("Índices de agregação", style = "text-align: center;"),
                                           br(),
                                           DT::dataTableOutput("agreg")
                                         )
                                         
                                ), # Agregação tab
                                
                                # Estrutura ####
                                tabPanel("Análise Estrutural",
                                         
                                         fluidPage(
                                           h1("Análise estrutural", style = "text-align: center;"),
                                           br(),
                                           fluidRow( 
                                             tabsetPanel(id = "mainPanel_Estrutural",
                                                         tabPanel("Análise estrutural", DT::dataTableOutput("estr") ),
                                                         tabPanel("Gráfico IVI", plotOutput("estrg",height = "500px" ) ) )
                                             ) #,
                                         #  fluidRow( column(width=4,uiOutput("rb_graphmsim"),offset = 3 ), 
                                        #             column(width=3,uiOutput("slider_graphmsim")) )
                                           
                                         )
                                
                                         
                                ) # Estrutura tab
                                
                                
                     ),# navbarMenu fitossociologica end ####
                     
                     # navbarMenu Quantificacao ####
                     navbarMenu( "Quantificação",
                                 
                       tabPanel("Distribuição diamétrica"),
                       tabPanel("BDq"),
                       tabPanel("Estrutura volumétrica"),
                       tabPanel("Inventário")
                       
                     ),  # navbarMenu Quantificacao end ####
                     
                     # navbarMenu  Download ####
                     navbarMenu( "Download",
                       
                       tabPanel("Download de tabelas"),
                       tabPanel("Download de Gráficos")
                       
                     ) # navbarMenu  Download end ####
                     
                     # final da UI  ####    
                     ) # navbarPage
  )#tagList
) # ShinyUI



