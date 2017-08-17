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
                     tabPanel("Mapeamento",
                     fluidPage(
                       
                       #h1("Shiny", span("Widgets Gallery", style = "font-weight: 300"), 
                       h1("Definição dos nomes das variáveis", 
                             
                          style = "text-align: center;"),
                       br(),
                       
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
                                h3("Espécie não-identificada"),
                                p("Selecione o(s) indice(s) referente(s) às espécies não identificadas:"#, 
                                  #style = "font-family: 'Source Sans Pro';"
                                ),
                                uiOutput("selec_rotuloNI")
                              ))
                       
                       
                       ), # fluidRow 1
                       
                       fluidRow(
                        
                         column(4,
                                wellPanel(
                                  h3("Diâmetro (DAP)"),
                                  p("Selecione o nome da variável referente à DAP:"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_dap")
                                )), # Coluna dap
                         
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
                                  h3("Volume"),
                                  p("Selecione o nome da variável referente à Volume:"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_vol")
                                )) # Coluna vol
                         
                         
                         
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
                       
                       
                     ) # fluidRow 3  
                       
                       
                     ) # fluidPage 
                     
                     
                     )# tabPanel Mapeamento

                     # final da UI  ####    
                     ) # navbarPage
  )#tagList
) # ShinyUI