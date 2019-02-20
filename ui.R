options(shiny.sanitize.errors = FALSE)
library(shiny)
suppressPackageStartupMessages(library(DT))
#library(plotly)
library(formattable)
library(readxl)
#library(plyr)
library(tibble)
library(tidyr)
suppressPackageStartupMessages(library(dplyr))
library(lazyeval)
suppressPackageStartupMessages(library(ggplot2))
library(ggdendro)
library(ggthemes)
library(openxlsx)
library(rmarkdown)
library(stringr)
library(googledrive)

inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}

inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}

shinyUI(
  # Intro, taglists e error messages colors ####
  tagList(tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar{background: #00a90a}")), # this is actually .css; this changes the color for the sliders
          
          # Timeout: depois de 20 minutos (12000000 milisegundos) fecha a aba do navegador
          tags$script(
            "function idleTimer() {
  var t = setTimeout(logout, 5000);
            window.onmousemove = resetTimer; // catches mouse movements
            window.onmousedown = resetTimer; // catches mouse movements
            window.onclick = resetTimer;     // catches mouse clicks
            window.onscroll = resetTimer;    // catches scrolling
            window.onkeypress = resetTimer;  //catches keyboard actions
            
            function logout() {
            process.exit();  //close the window
            }
            
            function resetTimer() {
            clearTimeout(t);
            t = setTimeout(logout, 12000000);  // time is in milliseconds (1000 is 1 second)
            }
            }
            idleTimer();"
            
          ),
          
          
          # Cor de todas as mensagens da funcao need
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-validation {
                            color: #00a90a;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "WRONG"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-WRONG {
                            color: red;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "AVISO"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-AVISO {
                            color: orange;
                            }
                            "))
          ),
          
          
          
          # Version ####
          navbarPage("App Inventário de Nativas 2.1.5",id="tab",
          #         ####           
                     theme = "green_yeti2.css",
                     # theme = "green.css", # seleciona um tema contido na pasta www
                     # theme = shinythemes::shinytheme("paper"), # seleciona um tema utilizando pacote
                     
                     # Painel Intro ####   
          
          
                     tabPanel( "Intro" ,
                               
                               # logging ####
                               inputIp("ipid"),
                               inputUserid("fingerprint"),
                               # ####
                               
                               
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
                                               c("Fazer o upload", 
                                                 "Utilizar o dado de exemplo em nivel de fuste",
                                                 "Utilizar o dado de exemplo em nivel de arvore" ), 
                                               selected = "Fazer o upload"),
                                  
                                  radioButtons("df", 
                                               "Tipo da base de dados:", 
                                               choices = c("Dados em nivel de fuste",
                                                           "Dados em nivel de arvore",
                                                           "Dados em nivel de parcela"),
                                               selected = "Dados em nivel de arvore"),

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
                      
                      fluidRow( # fluidRow 1 start
                        column(4,
                               wellPanel(
                                 h3("Árvore"),
                                 p("Selecione o nome da variável referente à 'Árvore':"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_arvore")
                               )), # Coluna Espécie
                        

                        column(4,
                               wellPanel(
                                 h3("Parcela*"),
                                 p("Selecione o nome da variável referente à 'Parcela':"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_parcelas")
                               )),
                        
                        column(4,
                               wellPanel(
                                 h3("Espécie*"),
                                 p("Selecione o nome da variável referente à 'Espécie':"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_especies")
                               )) # Coluna Espécie,
                        
                        
                      ), # fluidRow 1 end

                      fluidRow(  # fluidRow 2 start 
                        
                        column(4,
                               wellPanel(
                                 h3("Circunferência (CAP)*"),
                                 p("Selecione o nome da variável referente à 'CAP':"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_cap")
                               )), # Coluna Espécie
                        
                        
                        column(4,
                               wellPanel(
                                 h3("Diâmetro (DAP)*"),
                                 p("Selecione o nome da variável referente à 'DAP':"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_dap")
                               )), # Coluna dap
                        column(4,
                               wellPanel(
                                 h3("Altura total"),
                                 p("Selecione o nome da variável referente à 'Altura total':"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_ht")
                               )) # Coluna ht
                        
                      ), # fluidRow 2 end
                      
                       fluidRow( # fluidRow 3 start
                        
                         
                         
                         column(4,
                                wellPanel(
                                  h3("Volume com casca"),
                                  p("Selecione o nome da variável referente à 'Volume com casca':"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_vcc")
                                )), #, # Coluna vcc
                         
                         
                         column(4,
                                wellPanel(
                                  h3("Área da parcela"),
                                  p("Selecione o nome da variável referente à 'Área da parcela (m²)':"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_area.parcela")
                                )), # Coluna area.parcela
                         
                         
                         column(4,
                                wellPanel(
                                  h3("Área total"),
                                  p("Selecione o nome da variável referente à 'Área total (ha)'"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                                  ),
                                  uiOutput("selec_area.total")
                                )) # Coluna area.total
                         
                         
                      #   column(4,
                      #          wellPanel(
                      #            h3("Volume sem casca"),
                       #           p("Selecione o nome da variável referente à Volume sem casca:"#, 
                                    #style = "font-family: 'Source Sans Pro';"
                        #          ),
                       #           uiOutput("selec_vsc")
                       #         )) # Coluna vsc
                         
                         
                         
                       ),# fluidRow 3 end
                       
                      fluidRow( # fluidRow 4 start
                        
                        
                        column(4,
                               wellPanel(
                                 h3("Estrutura vertical"),
                                 p("Selecione o método de definição da 'Estrutura vertical'"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 radioButtons("est.vert.calc", 
                                              "Deseja definir a posição sociológica com base na variável altura, 
                                              ou inserir uma variável referente à posição sociológica?", 
                                              c("Definir", "Inserir"), 
                                              inline = T,
                                              selected = "Inserir"
                                 ),
                                 uiOutput("selec_est.vertical_2"),
                                 uiOutput("selec_est.vertical_warning")
                               )), # Coluna area.total
                        
                        
                        column(4,
                               wellPanel(
                                 h3("Estrutura interna"),
                                 p("Selecione o nome da variável referente à 'Estrutura interna'"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_est.interna")
                               )), # Coluna area.total
                        
                        column(4,
                               wellPanel(
                                 h3("Estrato"),
                                 p("Selecione o nome da variável referente à 'Estrato'"#, 
                                   #style = "font-family: 'Source Sans Pro';"
                                 ),
                                 uiOutput("selec_estrato")
                               ))                       
                        
                        
                        
                      ) # fluidRow 4 end
                      
                      

                     
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
                                    
                                    uiOutput("selec_area_parcela_num"),
                                    uiOutput("selec_area_total_num"),
                                    uiOutput("ui_estvcc1"),
                                    uiOutput("ui_estvcc3"),
                                    uiOutput("ui_estvcc4"),
                                    uiOutput("checkbox_calc.est.vert"),
                                    
                                    h3("Diâmetro mínimo"),
                                    numericInput("diam.min", "Insira o diâmetro mínimo:", 0, 0, 100, 1),
                                    
                                    h3("Intervalo de classe"),
                                    numericInput("int.classe", "Insira o intervalo de classe:", 5, 1, 50, 0.5),
                                    
                                    uiOutput("selec_rotuloNI"),

                                    h3("Filtrar dados"),
                                    
                                    uiOutput("rm_data_var"),
                                    uiOutput("rm_data_level"),
                                    uiOutput("rm_vars")
                                    
                                    
                                    
                                  ),# sidebarPanel
                                  
                                 mainPanel( tabsetPanel(
                                    tabPanel("Dado pos preparação",
                                             shiny::htmlOutput("avisos_prep"),
                                             DT::dataTableOutput("prep_table"),
                                             hr(),
                                             tableOutput("teste")
                                             ),
                                    tabPanel("Consistência dos dados",
                                             
                                             radioButtons(
                                               "run_consist",
                                               h3("Deseja verificar a consistência dos dados?"),
                                               choices = c("Sim"=TRUE,"Nao"=FALSE),
                                               selected=FALSE,
                                               inline = TRUE,
                                               width = "200%"),
                                             p("Obs: A consistência requer que a variável DAP esteja mapeada. Recomenda-se mapear também a variável Altura."),
                                             
                                             uiOutput("consist_warning1"),
                                             uiOutput("consist_warning2"),
                                             uiOutput("consist_table_help"),
                                             uiOutput("consist_choice"),
                                             DT::dataTableOutput("consist_table")
                                             )

                                  ))# mainPanel
                                  
                                  
                                )
                                
                              )
                              
                              
                              
                              
                     ), # tabPanel preparacao dados
                     # Totalização de fustes ####
                     tabPanel("Totalização de fustes",
                              
                              fluidPage(
                                
                                h1("Totalização de fustes", style = "text-align: center;"),
                                br(),
                                
                                DT::dataTableOutput("tot_fuste_tab")
                              )
                     ),# tabpanel Totalização de fustes
                     
                     
                     
                     
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
                                                                 h3("Calcular diversidade por grupo?"),
                                                                 c("Nao", "Parcela", "Estrato"),
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
                                           
                                           h1("Índices de similaridade", style = "text-align: center;"),
                                           br(),
                                           
                                           fluidRow(
                                             column(width=2,
                                                    radioButtons("rb_sim",
                                                                 h4("Similaridade entre:"),
                                                                 c("Parcela", "Estrato"),
                                                                 "Parcela",
                                                                 TRUE) ),
                                             
                                             column(width=3,
                                                    h3("Configuração dos gráficos:") ),
                                             
                                             column(width=5,
                                                    radioButtons("rb_msim_graph", 
                                                                 h4("Método de classificação:"), 
                                                                 c("Vizinho mais próximo"  = "single", 
                                                                   "Vizinho mais distante" = "complete", 
                                                                   "Distância euclidiana"  = "average"), 
                                                                 selected = "complete", inline = T)  ), 
                                             
                                             
                                             column(width=2,
                                                    sliderInput("slider_msim_graph", 
                                                                label = h4("Número de grupos:"), 
                                                                min = 1, 
                                                                max = 10, 
                                                                value = 3,
                                                                step = 1) )
                                           ),
                                           
                                           
                                           fluidRow( 
                                             tabsetPanel(id = "mainPanel_Indices",
                                                         tabPanel("Matriz de Similaridadede de Jaccard", DT::dataTableOutput("msim1") ),
                                                         tabPanel("Dendrograma Jaccard", plotOutput("msim1_graph_",height = "600px"), value="id_msim1_graph" ),
                                                         tabPanel("Matriz de Similaridadede de Sorensen", DT::dataTableOutput("msim2") ),
                                                         tabPanel("Dendrograma Sorensen", plotOutput("msim2_graph_",height = "600px"), value="id_msim2_graph" ) 
                                                         
                                             ) )
                                             
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
                                           fluidRow( uiOutput("ivi_graph_opts")
                                                        ),
                                           fluidRow( 
                                             tabsetPanel(id = "mainPanel_Estrutural",
                                                         tabPanel("Análise estrutural", DT::dataTableOutput("estr") ),
                                                         tabPanel("Gráfico IVI", plotOutput("estrg",height = "550px" ) ),
                                                         tabPanel("Gráfico estrutura vertical", plotOutput("est.vert_plot", height = "550px")) )
                                             ) #,
                                         #  fluidRow( column(width=4,uiOutput("rb_graphmsim"),offset = 3 ), 
                                        #             column(width=3,uiOutput("slider_graphmsim")) )
                                           
                                         )
                                
                                         
                                ) # Estrutura tab
                                
                                
                     ),# navbarMenu fitossociologica end ####
                     
                     # navbarMenu Quantificacao ####
                     navbarMenu( "Quantificação",
                       # tabPanel DD ####
                       tabPanel("Distribuição diamétrica",
                                
                                fluidPage(
                                  h1("Distribuição diamétrica (DD)", style = "text-align: center;"),
                                  br(),
                                  tabsetPanel(
                                    
                                    tabPanel("Distribuição diamétrica Geral", DT::dataTableOutput("dd_geral_tab") ),
                                    tabPanel("Distribuição diamétrica dos indivíduos por ha por espécie", DT::dataTableOutput("dd_indv_especie_tab") ),
                                    tabPanel("Distribuição diamétrica do volume por ha por espécie", DT::dataTableOutput("dd_vol_especie_tab") ),
                                    tabPanel("Distribuição diamétrica de G por ha por espécie", DT::dataTableOutput("dd_G_especie_tab") ),
                                    tabPanel("Gráfico dos indivíduos por ha por classe diamétrica", plotOutput("dd_graph_indv",height = "550px") ),
                                    tabPanel("Gráfico do volume por ha por classe diamétrica", plotOutput("dd_graph_vol",height = "550px")),
                                    tabPanel("Gráfico de G por ha por classe diamétrica", plotOutput("dd_graph_G",height = "550px")) 
                                  )
                                  
                                )
                                
                                ), # tabPanel DD 
                       
                       # tabPanel BDq ####
                       tabPanel("BDq",
                                
                                fluidPage(
                                  h1("BDq Meyer", style = "text-align: center;"),
                                  br(),
                                  fluidRow(column(width=5,
                                                  sliderInput("i.licourtBDq", 
                                                              label = "Valor do quociente de Licourt inicial:", 
                                                              min = 0, 
                                                              max = 5, 
                                                              value = 1.3,
                                                              step = .1),
                                                  offset=7
                                  )),
                                  tabsetPanel(
                                    
                                    tabPanel("BDq", DT::dataTableOutput("BDq1") ),
                                    tabPanel("Gráfico", plotOutput( "BDq_graph_" ,height = "550px") ),
                                    tabPanel("Detalhes do ajuste", DT::dataTableOutput("BDq3", "70%") )
                                    
                                  )
                                  
                                  
                                  
                                )
                                
                                ),# tabPanel BDq
                       # tabPanel Estrutura volumétrica ####
                      # tabPanel("Estrutura volumétrica"),
                       
                       # tabPanel inventario florestal ####
                       tabPanel("Inventário florestal",
                                
                                fluidPage(
                                  
                                              h1("Inventário florestal", style = "text-align: center;"),
                                              br(),
                                              
                                              # ####
                                              
                                              fluidRow(
                                                
                                                column(2,
                                                       sliderInput("alpha_inv", 
                                                                   label = "Nível de significância:", 
                                                                   min = 0.01, 
                                                                   max = 0.10, 
                                                                   value = 0.10,
                                                                   step = 0.01)
                                                ),
                                                
                                                column( 2,  sliderInput("erro_inv", 
                                                                        label = "Erro admitido (%):", 
                                                                        min = 1, 
                                                                        max = 20, 
                                                                        value = 10,
                                                                        step = 1)),
                                                
                                                column(2,
                                                       sliderInput("cd_inv", 
                                                                   label = "Número de casas decimais:", 
                                                                   min = 0, 
                                                                   max = 10, 
                                                                   value = 4,
                                                                   step = 1)
                                                ),
                                                
                                                column(2,
                                                       radioButtons(
                                                         inputId='pop_inv', # Id
                                                         label='População:', # nome que sera mostrado na UI
                                                         choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
                                                         selected="inf",
                                                         inline = T)
                                                ),
                                                
                                                column(3,
                                                       uiOutput("acs_estrato_rb"), uiOutput("as_estrato_rb"),
                                                       uiOutput("acs_as_warning")
                                                )
                                                
                                              ),
                                              
                                              fluidRow(
                                                radioButtons("yi_inv",
                                                             label="Variável utilizada nas estatísticas:",
                                                             choices = c("Indv", "G","VCC"),
                                                             selected = "VCC",
                                                             inline=T )
                                              ),
                                          fluidRow(   
                                          tabsetPanel(id="tabset_inv",
                                          tabPanel("Totalização de parcelas",DT::dataTableOutput("tot_parc_tab") ) , 
                                          tabPanel("Amostragem casual simples",DT::dataTableOutput("acs") ), 
                                          tabPanel("Amostragem casual estratificada",DT::dataTableOutput("ace1"),br(),DT::dataTableOutput("ace2") ), 
                                          tabPanel("Amostragem sistemática",DT::dataTableOutput("as") ) )
                                        )
                                  # ####
                                    
                                  )
                                  
                                  
                                
                                
                                )# TabPanel Inventario
                       
                       
                     ),  # navbarMenu Quantificacao end ####
                     
                     # navbarMenu  Download ####
                     tabPanel("Download",
                                # Painel Download Tabelas ####
                                
                              fluidPage(
                                
                                
                                h1("Download dos resultados", style = "text-align: center;"),
                                br(),
                                
                                
                              tabsetPanel(
                                tabPanel("Download de tabelas", 
                                         fluidPage(
                                           
                                     
                                           h2("Download de tabelas", style = "text-align: center;"),
                                           br(),
                                           
                                           helpText(
                                             "Ao clicar no botão de download, você se declara de acordo com os termos descritos",
                                             a(href="https://docs.google.com/document/d/1nvPcNTHCZJhuqsEYoHdYR9NVc44_AJuaHUynQwveVgk/edit?usp=sharing", "aqui"),
                                             "."
                                           ),
                                           
                                             fluidRow(
                                               column(
                                                 10
                                                 ,uiOutput("checkbox_df_download")
                                                 )
                                               
                                                      ),
                                           br(),
                                           
                                           fluidRow(column(3,downloadButton('downloadData', 'Baixar tabelas selecionadas'), offset=4)),
                                           br(),

                                            h3("Ou, para baixar todas as tabelas disponíveis, clique abaixo:"),
                                           fluidRow(
                                             column(3,downloadButton('downloadAllData', 'Baixar todas as tabelas'), offset=4)
                                             )
                                           
                                           

                                         )
                                ), # download tabelas
                                
                                # Painel Download Graficos ####
                                
                                tabPanel("Download de graficos", 
                                         
                                         
                                         
                                         sidebarLayout(
                                           
                                           sidebarPanel(
                                             
                                             tags$style(type="text/css",
                                                        ".recalculating {opacity: 1.0;}"
                                             ),
                                             
                                             h3("Download de graficos"),
                                             
                                             helpText(
                                               "Ao clicar no botão de download, você se declara de acordo com os termos descritos",
                                               a(href="https://docs.google.com/document/d/1nvPcNTHCZJhuqsEYoHdYR9NVc44_AJuaHUynQwveVgk/edit?usp=sharing", "aqui"),
                                               "."
                                             ),
                                             
                                             selectInput("graph_d", "Escolha um grafico:", 
                                                         choices = c(
                                                           "Dendrograma - Jaccard",
                                                           "Dendrograma - Sorensen",
                                                           "Grafico IVI",
                                                           "Grafico Estrutura Vertical",
                                                           "Indv. por ha por CC",
                                                           "Vol. por ha por CC",
                                                           "G por ha por CC",
                                                           "Distribuicao - BDq Meyer" )),
                                             
                                             selectInput("graphformat",
                                                         "Escolha o formato do gráfico:",
                                                         choices = c("PNG" = ".png",
                                                                     "JPG" = ".jpg",
                                                                     "PDF" = ".pdf") ),
                                             
                                             downloadButton('downloadGraph', 'Download')
                                             
                                           ),
                                           mainPanel(
                                             plotOutput("graph_d_out",height = "550px")
                                           )
                                         )
                                ) # download graficos
                                
                                )       
                     ) # fluidPage
                     ) # final navbarMenu download ####    
                     # final da UI  ####    
                     ) # navbarPage
  )#tagList
) # ShinyUI



