library(shiny)
library(DT)
#library(plotly)
library(formattable)
library(readxl)
library(plyr)
library(tibble)
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

# Data e functions

ex <- read.csv("examples/Inventory_exemplo.csv",fileEncoding="UTF-8")
#ex <- read.csv("examples/Inventory_exemplo_2.csv",fileEncoding="UTF-8")

source("funs/diversidade.R"        , encoding="UTF-8")
source("funs/pareadoSimilaridade.R", encoding="UTF-8")
source("funs/matrizSimilaridade.R" , encoding="UTF-8")
source("funs/estrutura.R"          , encoding="UTF-8")
source("funs/BDq.R"                , encoding="UTF-8")
source("funs/agregacao.R"          , encoding="UTF-8")
source("funs/acs.R"                , encoding="UTF-8")
source("funs/ace.R"                , encoding="UTF-8")
source("funs/as_diffs.R"           , encoding="UTF-8")
source("funs/inv_summary.R"        , encoding="UTF-8")
source("funs/round_df.R"           , encoding="UTF-8")

# vectors for names ####

especies_names <- c("nome.cient","scientific.name","Scientific.Name","SCIENTIFIC.NAME" ,"scientific_name", "Scientific_Name","SCIENTIFIC_NAME","nome.cientifico", "Nome.Cientifico","NOME.CIENTIFICO","nome_cientifico", "Nome_Cientifico","NOME_CIENTIFICO")
parcelas_names <- c("transecto","transect", "Transect", "TRNASECT", "transect.code","Transect.Code","TRANSECT.CODE","transect_code","Transect_Code","TRANSECT_CODE","parcela", "Parcela","PARCELA","cod.parcela","Cod.Parcela","COD.PARCELA", "cod_parcela","Cod_Parcela","COD_PARCELA")
est.vertical_names <- c("pos.copa","canopy", "canopy_09")
est.interno_names <- c("luminosidade","light", "light_09")


DAP_names <- c("DAP","Dap","dap", "dbh", "Dbh","DBH","DBH_11")
HT_names <- c("HT_EST", "HT", "Ht", "ht","Htot","ALTURA","Altura","Altura_Total", "ALTURA_TOTAL")
VCC_names <- c("VCC","Vcc", "vcc", "VOL", "Vol", "vol" ,"VOLUME")
area_parcela_names <- c("trans.area","AREA_PARCELA","Area_Parcela","area_parcela","parc.area" ,"AREAPARCELA", "areaparcela", "transect.area", "Transect.Area", "TRANSECT.AREA","transect_area","Transect_Area","TRANSECT_AREA")
area_total_names <- c("sub.area","AREA_TOTAL", "AREATOTAL", "area_total", "areatotal","AREA_TALHAO", "AREATALHAO", "area_talhao", "areatalhao","total.area","Total.Area","TOTAL.AREA","total_area","Total_Area","TOTAL_AREA")
idade_names <- c("IDADE", "Idade","idade")
VSC_names <- c("VSC","Vsc", "vsc")
HD_names <- c("HD", "Hd", "hd", "ALTURA_DOMINANTE", "ALT_DOM")
grupos_names <- c(c("TALHAO", "PARCELA"), c("area.code", "transect"), c("codigo", "transecto"), "parcela", "PARCELA", "transect", "cod.parcela", "Cod.parcela", "COD.PARCELA")
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")

# Server ####

shinyServer(function(input, output, session) {
  
  # Importar os dados ####

  output$upload <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload", "" )  )

    list(    
     
      radioButtons("df", 
                   "Tipo da base de dados:", 
                   choices = c("Dados em nivel de arvore",
                               "Dados em nivel de parcela"),
                   selected = "Dados em nivel de arvore"),
      
      
      radioButtons("df_extension", 
                   "Informe o formato do arquivo:", 
                   choices = c(".csv (Valor separado por virgulas) ou .txt (arquivo de texto)",
                               ".xlsx (Excel)"),
                   selected = ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)")
      
      
    )
    
    
  })
  
  output$upload_csv <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)", "" )  )
    
    list(    
      
       radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='sep',  #Id
        label='Separador:', # nome que sera mostrado na UI
        choices=c(Virgula=',', "Ponto e Virgula"=';', Tabulação='\t'), # opcoes e seus nomes
        selected=','), # valor que sera selecionado inicialmente
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='dec', # Id
        label='Decimal:', # nome que sera mostrado na UI
        choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
        selected="."), # valor que sera selecionado inicialmente
      
      fileInput( # input de arquivos
        inputId = "file1", # Id
        
        label = "Selecione o arquivo: (.csv ou .txt)", # nome que sera mostrado na UI
        
        accept=c('text/csv', ".txt",'.csv'))
      
      

      
      
      
    )
    
    
  })
  
  output$upload_xlsx <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".xlsx (Excel)", "" )  )
    
    list(    
      # Selecionar numero da planilha
      numericInput(inputId = "sheet_n",
                   label   = "Número da planilha",
                   value   = 1,
                   min     = 1,
                   max     = 30,
                   step    = 1
      ),
      
      radioButtons(inputId = "mv_excel",label = "Valores ausentes", choices = c("Espaço vazio" = "", "NA" = "NA"), inline = T ),
      
      # input de arquivos
      fileInput( 
      inputId = "file2", # Id
      
      label = "Selecione o arquivo: (.xlsx)", # nome que sera mostrado na UI
      
      # So aceita .xlsx
      accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                 '.xlsx')),
        
          
      div("Recomendamos o uso do formato .csv", style = "color:blue")
  
      
    )
    
    
  })
  
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
  
    # sera nulo caso nao sejam selecionados "fazer o upload"
    validate(need(input$df_select == "Fazer o upload" , NULL )  )
    
    # Salva o caminho do arquivo uploadado em um arquivo, dependendo do que o usuario selecionar
    if(input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)"){
      inFile <- input$file1
    }else if( input$df_extension == ".xlsx (Excel)"){
      inFile <- input$file2
    } # caso contrario, salvar o caminho do arquivo carregado em inFile
    
    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    if (is.null(inFile) ){return(NULL)} # se o arquivo nao for carregado, retornar null
    else if(input$df_extension != ".xlsx (Excel)")
    {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {
      file.copy(inFile$datapath,
                      paste(inFile$datapath, "xlsx", sep="."))
      raw_data <-  readxl::read_excel(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n, na = input$mv_excel) 
      raw_data <- as.data.frame(raw_data)
      }
    
    # Carregamos o arquivo em um objeto
    
    
    raw_data # tabela final a ser mostrada. 
    
  })
  
  # rawData_ (com traco) sera o dado bruto sem filtro. Este dataframe sera utilizado em todo o app
  rawData_ <- reactive({
    
    # raw data, sera definido como o exemplo, ou o dado de upload, dependendo do usuario.
    # para evitar erros, caso seja selecionado "Fazer o upload" mas o dado ainda não tenha sido uploadado,
    # sera retornanado vazio
    switch(input$df_select, 
           "Fazer o upload" = if(is.null(input$file1) && is.null(input$file2)){return()}else{upData()},
           "Utilizar o dado de exemplo" = ex)
    
  })
  
  output$rawdata <- renderDataTable({ # renderizamos uma DT::DataTable
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    data <- rawData_() 
    
    datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}")
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
  })

  # Filtrar dados ####
  
  # rawData (sem traco) sera o dado bruto com filtro, caso o usuario
  # rode algum filtro, caso contrario sera o dado bruto inalterado
  rawData <- reactive({
    
    data <- rawData_()
    
     # se o usuario nao selecionar nada, retorna o dado normal 
     # (isso faz com o que o dado original seja exibido logo que se entra na aba de filtrar),
     # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis

      if( is.null(input$filtrar_dados_col1) || input$filtrar_dados_col1 ==""){
      
        data
        
      }else{
        
        data <- data[!data[[input$filtrar_dados_col1]] %in% input$filtrar_dados_col1_filtro,]
        # data <- data %>% 
           #filter( ! .data[[input$filtrar_dados_col1]] %in% input$filtrar_dados_col1_filtro )
         data
        
      }
    
    # se o usuario nao selecionar nada, uma coluna vazia e definida como nula,
    # ou seja, nao muda nada no dado.
    # por isso nao e necessario utilizar condicionais nesse caso

      data[, input$filtrar_dados_rm_cols] <- NULL
      
      data

  })
  
  output$filtrar_dados_col1_ui <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "filtrar_dados_col1", # Id
      "Selecione a coluna que se deseja filtrar:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$filtrar_dados_col1_filtro_ui <- renderUI({
    
    if( is.null(input$filtrar_dados_col1) || input$filtrar_dados_col1 =="" ){
      
      opcoes <- NULL
      
      }else{
    
    data <- rawData_()
    
    opcoes <- levels(
      as.factor(
        data[,input$filtrar_dados_col1]))
      }
    
    selectizeInput("filtrar_dados_col1_filtro",
                   label = "Selecione o(s) nivel(s) que se deseja remover:",
                   choices = opcoes,
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione o(s) nivel(s) abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )
    
    
    
  })
  
  output$filtrar_dados_rm_cols_ui <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "filtrar_dados_rm_cols", # Id
      "Selecione a(s) coluna(s) que se deseja remover:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(" "); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$filter_table <- renderDataTable({
    
    data <- rawData()
    
    datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}")
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    
  })

  # Índices de diversidade ####
  
  # funcao diversidade
  tabdiversidade <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesdiv != "","Por favor selecione a coluna referente a 'especies'  ") )
    
    {
      
      dados <- rawData()
      
      x <- diversidade(data             = dados, 
                       col.especies     = input$col.especiesdiv, 
                       col.parcelas     = input$col.parcelasdiv, 
                       rotulo.NI        = input$rotutuloNIdiv  ) # %>% 
        #gather("Índice", "Resultado") # transpor tabela
      
      x 
    }
    
  })
  
  # UI
  output$selec_especiesdiv <- renderUI({
    
    data <- rawData()
    
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        "col.especiesdiv", # Id
        "Selecione a coluna de espécies:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = especies_names,
        multiple=T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          #onInitialize = I('function() { this.setValue(""); }')
        ) # options    
      ) # selctize

      # obs: multiple = T & maxItems = 1, garantem que a celula fique vazia, caso o app falhe
      # em tentar adivinhar o nome da especie
  })
  
  output$selec_rotuloNIdiv <- renderUI({
    
    validate(need(input$col.especiesdiv != "","") )
    
    data <- rawData()
    
    selectizeInput("rotutuloNIdiv",
                   label = "Selecione o(s) código(s) de indivíduo(s) não indentificado(s):",
                   choices = levels(as.factor(data[,input$col.especiesdiv])),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione um ou mais rótulos abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) )

  })
  
  output$selec_parcelasdiv <- renderUI({
    
    data <- rawData()

    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasdiv", # Id
      "Selecione a coluna de parcelas:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = parcelas_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  # tabela
  output$div <- renderDataTable({
    
      divdt <- tabdiversidade() 
      
      datatable( divdt,
                 options = list(searching = FALSE,
                                paging=FALSE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )   
      ) 

  }) 
  
  # Matriz Similaridade ####
  
  # funcao m similaridade
  tabmsimilaridade1 <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesmsim!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelasmsim!= "","Por favor selecione a coluna referente a 'parcelas' ") 
    )
    
    dados <- rawData()
      
      x <- m.similaridade(data             = dados, 
                          col.especies     = input$col.especiesmsim,
                          col.comparison   = input$col.parcelasmsim,
                          rotulo.NI        = input$rotutuloNImsim  )
      
      x <- as.data.frame(x[[1]])
      names(x) <- 1:length(x)
      x
    
    
  })
  tabmsimilaridade2 <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesmsim!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelasmsim!= "","Por favor selecione a coluna referente a 'parcelas' ") 
    )

      dados <- rawData()
      
      x <- m.similaridade(data             = dados, 
                          col.especies     = input$col.especiesmsim,
                          col.comparison   = input$col.parcelasmsim,
                          rotulo.NI        = input$rotutuloNImsim  )
      
      x <- as.data.frame(x[[2]])
      names(x) <- 1:length(x)
      x

  })
  
  # UI
  
  output$selec_especiesmsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiesmsim", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names,
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_parcelasmsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasmsim", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_rotuloNImsim <- renderUI({
    
    validate(need(input$col.especiesmsim != "","") )
    
    data <- rawData()

    selectizeInput("rotutuloNImsim",
                   label = "Selecione o(s) código(s) de indivíduo(s) não indentificado(s):",
                   choices = levels(as.factor(data[,input$col.especiesmsim])),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione um ou mais rótulos abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) )

  })
  output$rb_slider_graphmsim1 <- renderUI({
    
    # precisa que o grafico seja selecionado na ui, caso contrario nao mostra nada
    validate(need(input$mainPanel_Indices == "id_msim1_graph", "" )  ) 
    
   list( 
     
     radioButtons("rb_msim1_graph", 
                  "Selecione o método de classificação:", 
                  c("Vizinho mais próximo"  = "single", 
                    "Vizinho mais distante" = "complete", 
                    "Distância euclidiana"  = "average"), 
                  selected = "complete"),
     
     sliderInput("slider_msim1_graph", 
                     label = "Selecione o número de clusters:", 
                     min = 0, 
                     max = 10, 
                     value = 3,
                     step = 1) )
    
  })
  output$rb_slider_graphmsim2 <- renderUI({
    # precisa que o grafico seja selecionado na ui, caso contrario nao mostra nada
        validate(need(input$mainPanel_Indices == "id_msim2_graph", "" )  )
    
    list(  
      
      radioButtons("rb_msim2_graph", 
                   "Selecione o método de classificação:", 
                   c("Vizinho mais próximo"  = "single", 
                     "Vizinho mais distante" = "complete", 
                     "Distância euclidiana"  = "average"), 
                   selected = "complete"),
      
    
    sliderInput("slider_msim2_graph", 
                label = "Selecione o número de clusters:", 
                min = 0, 
                max = 10, 
                value = 3,
                step = 1) )
    
  }) 
  
  
  # tabela
  output$msim1 <- renderDataTable({

 
    {
      msimdt1 <- tibble::rownames_to_column(tabmsimilaridade1(), " ") 
      
      datatable( msimdt1,
                 rownames = F,
                 options = list(searching = FALSE,
                                paging=FALSE,
                                ordering=FALSE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )   
           ) %>% 
        formatStyle(1, backgroundColor = "#00a90a", color = '#fff' )
    }
    
  }) 
  output$msim2 <- renderDataTable({
    
    {
      msimdt2 <- tibble::rownames_to_column(tabmsimilaridade2(), " ") 
      
      datatable( msimdt2,
                 rownames = F,
                 options = list(searching = FALSE,
                                paging=FALSE,
                                ordering=FALSE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )   
      ) %>% 
        formatStyle(1, backgroundColor = "#00a90a", color = '#fff' )
    }
    
  }) 
  
  # graficos 
  msim1_graph <- reactive({
    
    #retornar vazio enquando input$rb_msim1_graph carrega (ele fica nulo quando carrega)
    if(is.null(input$rb_msim1_graph)){return("")} 
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesmsim!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelasmsim!= "","Por favor selecione a coluna referente a 'parcelas' ") 
    )
    {
      dados <- rawData()
      df <- as.data.frame(tabmsimilaridade1() ) 
      
      rownames(df) <- levels( as.factor( dados[,input$col.parcelasmsim] ) )
      
      hc    <- hclust(dist(df), input$rb_msim1_graph) # heirarchal clustering
      dendr <- ggdendro::dendro_data(hc) # convert for ggplot
      clust    <- cutree(hc,k=input$slider_msim1_graph)                    # find 2 clusters
      clust.df <- data.frame(label=names(clust), cluster=factor(clust))
      
      # dendr[["labels"]] has the labels, merge with clust.df based on label column
      dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
      # plot the dendrogram; note use of color=cluster in geom_text(...)

      x <- ggdendro::ggdendrogram(dendr) +
        geom_text(data=ggdendro::label(dendr), aes(x, y, label=label, hjust=.5,color=cluster), size=4) +
        ggdendro::theme_dendro()
      
      x
      
      }
    
    
    
  })
  output$msim1_graph_ <- renderPlot({
    
    gmsim1 <- msim1_graph()
    
    gmsim1
    
  })

  msim2_graph <- reactive({
    
    #retornar vazio enquando input$rb_msim1_graph carrega (ele fica nulo quando carrega)
    if(is.null(input$rb_msim2_graph)){return("")} 
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesmsim!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelasmsim!= "","Por favor selecione a coluna referente a 'parcelas' ") 
    )
    {
      dados <- rawData()
      df <- as.data.frame(tabmsimilaridade2() ) 
      
      rownames(df) <- levels( as.factor( dados[,input$col.parcelasmsim] ) )
      
      hc    <- hclust(dist(df), input$rb_msim2_graph) # heirarchal clustering
      dendr <- ggdendro::dendro_data(hc) # convert for ggplot
      clust    <- cutree(hc,k=input$slider_msim2_graph) 
      clust.df <- data.frame(label=names(clust), cluster=factor(clust))
      
      # dendr[["labels"]] has the labels, merge with clust.df based on label column
      dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
      # plot the dendrogram; note use of color=cluster in geom_text(...)
      
      x <- ggdendro::ggdendrogram(dendr) +
        geom_text(data=ggdendro::label(dendr), aes(x, y, label=label, hjust=.5,color=cluster), size=4) +
        ggdendro::theme_dendro()
      
      x   
      
      }
    
    
    
  })
  output$msim2_graph_ <- renderPlot({
    
   gmsim2 <- msim2_graph()
   
   gmsim2
    
    
  })
  
  # Pareado Similaridade ####
  # funcao p similaridade
  tabpsimilaridade <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiespsim!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelaspsim!= "","Por favor selecione a coluna referente a 'parcelas' "),
             need(input$psimselec_parc1!= "","Por favor selecione o primeiro item que se deseja comparar "),
             need(input$psimselec_parc2!= "","Por favor selecione o segundo item que se deseja comparar ")
             
    )
    
    dados <- rawData()
      
      #inv %>% 
      #filter_(.dots = interp(~ transect == "T01", transect = as.name("transect") ) ) %>% 
      #select_("scientific.name")
      
      x <- dados %>% 
        filter_(.dots = interp(~ transect == input$psimselec_parc1, transect = as.name(input$col.parcelaspsim) ) ) %>% 
        select_(input$col.especiespsim)
      
      y <- dados %>% 
        filter_(.dots = interp(~ transect == input$psimselec_parc2, transect = as.name(input$col.parcelaspsim) ) ) %>% 
        select_(input$col.especiespsim)
      
      x <- p.similaridade( 
        x         = x[,1],
        y         = y[,1],
        rotuloNI = input$rotutuloNIpsim  )
      
      x <- data.frame( "Índices" = c("Jaccard", "Sorensen")  ,
                       "Resultado" = c( x[1], x[2] )  )
      x
      

  })
  
  # UI
  
  output$selec_especiespsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiespsim", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names,     
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_parcelaspsim <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelaspsim", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  # cria lista com os nomes das parcelas
  lista_parcelas_psim <- reactive({
    
    if(is.null(input$col.parcelaspsim) || is.null(rawData()) ){return()}

    data <- rawData()
    
    parcelas <- unique(
      as.character(
        data[,input$col.parcelaspsim]))
    
    parcelas
    
  })
  
  output$selec_psimselec_parc1 <- renderUI({
    
    if(is.null(input$col.parcelaspsim) || is.null(rawData()) ){return()}
    
    data <- rawData()
    
    parcelas <- lista_parcelas_psim()

    
    selectizeInput("psimselec_parc1",
                   label = "Selecione o primeiro item que se deseja comparar:",
                   choices = parcelas,
                   options = list(
                     placeholder = 'Selecione um item abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )
    
  })
  
  output$selec_psimselec_parc2 <- renderUI({
    
    if(is.null(input$col.parcelaspsim) || is.null(rawData()) ){return()}
    
    data <- rawData()
    
    parcelas <- lista_parcelas_psim()
    
    selectizeInput("psimselec_parc2",
                   label = "Selecione o segundo item que se deseja comparar:",
                   choices = parcelas,
                   options = list(
                     placeholder = 'Selecione uma item abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )
    
  })
  
  output$selec_rotuloNIpsim <- renderUI({
    
    if(is.null(input$col.especiespsim)){return(NULL)}
    
    data <- rawData()
    
    selectizeInput("rotutuloNIpsim",
                   label = "Selecione o(s) código(s) de indivíduo(s) não indentificado(s):",
                   choices = levels(as.factor(data[,input$col.especiespsim])),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione um ou mais rótulos abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) )
    
    
  })
  
  # tabela
  output$psim <- renderDataTable({
    
      psimdt <- tabpsimilaridade() 
      
      datatable( psimdt,
                 options = list(searching = FALSE,
                                paging=FALSE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )  ) 

  }) 
 
  # Índices de agregacao ####
  
  tabagregate <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesagreg!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelasagreg!= "","Por favor selecione a coluna referente a 'parcelas' ")
    )
    
    dados <- rawData()
    
    x <- agregacao(data         =  dados, 
                   col.especies = input$col.especiesagreg, 
                   col.parcelas = input$col.parcelasagreg, 
                   rotulo.NI    = input$rotutuloNIagreg  )
    
    x

  })
  
  output$selec_especiesagreg <- renderUI({
    
    data <- rawData()
    
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiesagreg", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names, 
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
      )
    
  })
  
  output$selec_parcelasagreg <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasagreg", # Id
      "Selecione a coluna das parcelas:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
      )
    
  })
  
  output$selec_rotuloNIagreg <- renderUI({

    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesagreg!= "","Por favor selecione a coluna referente a 'especies' ")
    )
    
    data <- rawData()
    
    selectizeInput("rotutuloNIagreg",
                   label = "Selecione o(s) código(s) de indivíduo(s) não indentificado(s):",
                   choices = levels(as.factor(data[,input$col.especiesagreg])),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione um ou mais rótulos abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) )

  })
  
  output$agreg <- renderDataTable({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesagreg!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelasagreg!= "","Por favor selecione a coluna referente a 'parcelas' ")
    )
    
    agregdt <- tabagregate() 
      
      datatable( agregdt,
                 options = list(searching = T,
                                paging=T,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )  ) 

  }) 
  
  # Estrutura ####
  
  # funcao estrutura
  tabestrutura <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.especiesestr!= "","Por favor selecione a coluna referente a 'especies' "),
             need(input$col.parcelasestr!= "","Por favor selecione a coluna referente a 'parcelas' "),
             need(input$col.dapestr!= "","Por favor selecione a coluna referente a 'dap' "),
             need(input$area.parcelaestr!= "","Por favor selecione a coluna referente a 'area da parcela' ")
    )
    
      dados <- rawData()
      
      x <- estrutura(data             = dados, 
                     col.especies     = input$col.especiesestr,
                     col.parcelas     = input$col.parcelasestr,
                     col.dap          = input$col.dapestr,
                     area.parcela     = input$area.parcelaestr,
                     est.vertical     = input$est.verticalestr,
                     est.interno      = input$est.internoestr,
                     nao.identificada = input$rotutuloNIestr  )
      
      as.tbl(x)

  })
  
  # UI
  output$selec_especiesestr <- renderUI({
    
    data <- rawData()

    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especiesestr", # Id
      "Selecione a coluna de espécies:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = especies_names, 
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_parcelasestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasestr", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_dapestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dapestr", # Id
      "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,     
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_area.parcelaestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "area.parcelaestr", # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,     
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_rotuloNIestr <- renderUI({

    if(is.null(input$col.especiesestr)){return(NULL)}
    
    data <- rawData()
    
    selectizeInput("rotutuloNIestr",
                   label = "Selecione o(s) código(s) de indivíduo(s) não indentificado(s):",
                   choices = levels(as.factor(data[,input$col.especiesestr])),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione um ou mais rótulos abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) )
    
  })
  output$selec_est.verticalestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "est.verticalestr", # Id
      "Selecione a coluna estrutura vertical:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = est.vertical_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_est.internoestr <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "est.internoestr", # Id
      "Selecione a coluna estrutura interna:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      #selected = est.interno_names,     
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  # tabela
  output$estr <- renderDataTable({
    
      estrdt <- round_df( tabestrutura(), input$cdestr )
      
      datatable( as.tbl(estrdt),
                 options = list(searching = T,
                                paging=T,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )  ) 

  }) 
  
  
  # BDq Meyer ####
  
  # funcao BDq Meyer
  tabBDq1 <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.parcelasBDq!= "","Por favor selecione a coluna referente a 'parcelas' "),
             need(input$col.dapBDq!= "","Por favor selecione a coluna referente a 'dap' "),
             need(input$area.parcelaBDq!= "","Por favor selecione a coluna referente a 'area da parcela' ")
    )
    
      dados <- rawData()
      
      x <- bdq.meyer(data             = dados, 
                     col.parcelas     = input$col.parcelasBDq,
                     col.dap          = input$col.dapBDq,
                     area.parcela     = input$area.parcelaBDq,
                     intervalo.classe = input$intervalo.classeBDq,
                     min.dap          = input$min.dapBDq,
                     i.licourt        = input$i.licourtBDq  )
      
      x[[1]]

  })
  tabBDq3 <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.parcelasBDq!= "","Por favor selecione a coluna referente a 'parcelas' "),
             need(input$col.dapBDq!= "","Por favor selecione a coluna referente a 'dap' "),
             need(input$area.parcelaBDq!= "","Por favor selecione a coluna referente a 'area da parcela' ")
    )
      dados <- rawData()
      
      x <- bdq.meyer(data             = dados, 
                     col.parcelas     = input$col.parcelasBDq,
                     col.dap          = input$col.dapBDq,
                     area.parcela     = input$area.parcelaBDq,
                     intervalo.classe = input$intervalo.classeBDq,
                     min.dap          = input$min.dapBDq,
                     i.licourt        = input$i.licourtBDq  )
      
      x <-data.frame( "Coeficientes" = c("b0", "b1")  ,
                      "Valor"        = c( x[[3]][1], x[[3]][2] )  )
      x

  })
  
  # UI
  
  output$selec_parcelasBDq <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelasBDq", # Id
      "Selecione a coluna da parcela:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_dapBDq <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dapBDq", # Id
      "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names, 
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  output$selec_area.parcelaBDq <- renderUI({
    
    data <- rawData()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "area.parcelaBDq", # Id
      "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = area_parcela_names,   
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    )
    
  })
  
  # tabela
  output$BDq1 <- renderDataTable({
    
      BDqdt <- tabBDq1()
      
      datatable( as.data.frame(BDqdt),
                 options = list(searching = T,
                                paging=T,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )  ) 

  }) 
  output$BDq3 <- renderDataTable({
    
      BDqdt <- tabBDq3()

      datatable(BDqdt,
                 options = list(searching = FALSE,
                                paging=FALSE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )  ) 

  }) 

  # grafico
  BDq_graph <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$col.parcelasBDq!= "","Por favor selecione a coluna referente a 'parcelas' "),
             need(input$col.dapBDq!= "","Por favor selecione a coluna referente a 'dap' "),
             need(input$area.parcelaBDq!= "","Por favor selecione a coluna referente a 'area da parcela' ")
    )
    
      data <- tabBDq1()
      
      graph_bdq <- data %>% 
        select("classe_de_diametro"      = CentroClasse, 
               "Distribuição observada"  = IndvHectare , 
               "Distribuição balanceada" = MeyerBalan  ) %>% 
        gather(class, num_indv_ha, -classe_de_diametro, factor_key = T) %>% 
        arrange(classe_de_diametro) %>% 
        mutate(classe_de_diametro = as.factor(classe_de_diametro) )
      
      g <-  ggplot(graph_bdq, aes(x = classe_de_diametro, y = num_indv_ha) ) + 
        geom_bar(aes(fill = class), stat = "identity",position = "dodge") +
        labs(x = "Classe de diâmetro (cm)", y = "Número de indivíduos (ha)", fill = NULL) + 
        scale_fill_manual(values =c("#108e00", "cyan3","firebrick2") ) +
        theme(legend.position="bottom") +
        ggthemes::theme_hc(base_size = 14) 
      #ggthemes::theme_igray(base_size = 14)
      
      g

  })
  
  output$BDq_graph_ <- renderPlot({
    #plotly::renderPlotly
    if(is.null(BDq_graph() ) ) return(NULL)
    
    g <- BDq_graph()
    
  #  plotly::ggplotly(p=g) 
    g
    
  })
  
  # Inventario ####
      # Dado utilizado na totalizacao de parcelas ####
  totData <- reactive({
    
    # Primeiro testa-se se b1 nao e nulo, pois b1_estvol esta sendo criado no server,
    # e so e criado quando o codigo do server e rodado.
    # em seguida testa-se se b1_estvol e numerico, ou seja, se o usuario ja inseriu valores para ele.
    # Se ele nao for, o dado utilizado continuara sendo rawData.
    # Quando ele deixar de ser numerico, ou seja, quando tiver valor, ai sim este dado sera utilizado.
    if( is.null(input$b1_estvol) ){
      
     rawData()
      
    }else if(!is.numeric(input$b1_estvol)){
      
     rawData()
      
    }else{ 

     est_vol()
      }

    
  })
  
      # Calculo de volume ####

  est_vol <- reactive({
    
    data <- rawData()
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$DAP_estvol != "","Por favor insira o valor de 'DAP' "),
             need(input$bo_estvol != "","Por favor insira o valor de 'b0' "),
             need(input$b1_estvol != "","Por favor insira o valor de 'b1' ")
    )

    if(input$modelo_estvol == "LN(VFFC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e"){
    data$VOL <- exp( input$bo_estvol + log(data[[input$DAP_estvol]]) * input$b1_estvol + log(data[[input$HT_estvol]]) * input$b2_estvol )
    data <- data %>% select(VOL, everything())
    }
    
    if(input$modelo_estvol == "VFFC = b0 + b1 * DAP + b2 * HT + e"){
    data$VOL <- input$bo_estvol + data[[input$DAP_estvol]] * input$b1_estvol + data[[input$HT_estvol]] * input$b2_estvol
    data <- data %>% select(VOL, everything())
    }
    
    if(input$modelo_estvol == "LN(VFFC) = b0 + b1 * 1/DAP + e"){
    data$VOL <- exp( input$bo_estvol + 1/data[[input$DAP_estvol]] * input$b1_estvol )
    data <- data %>% select(VOL, everything())
    }
     
    if(input$modelo_estvol == "VFFC = b0 + b1 * DAP + e"){
    data$VOL <- input$bo_estvol + data[[input$DAP_estvol]] * input$b1_estvol
    data <- data %>% select(VOL, everything())
    }
    
    if(input$modelo_estvol == "VFFC = b0 + b1 * DAP² + e"){
      data$VOL <- input$bo_estvol + data[[input$DAP_estvol]]^2 * input$b1_estvol
      data <- data %>% select(VOL, everything())
    }
    
    if(input$modelo_estvol == "VFFC = b0 + b1 * DAP + b2 * DAP² + e"){
    data$VOL <- input$bo_estvol + data[[input$DAP_estvol]] * input$b1_estvol + data[[input$DAP_estvol]]^2 * input$b2_estvol
    data <- data %>% select(VOL, everything())
    }
    
    if(input$modelo_estvol == "VFFC = b0 + b1 * LN(DAP) + e"){
    data$VOL <- input$bo_estvol + log(data[[input$DAP_estvol]]) * input$b1_estvol
    data <- data %>% select(VOL, everything())
    
    }
    data
  })
  
  output$ui_estvol1 <- renderUI({

    data <- rawData()
    
    list(
      
      h3("Estimaçao de Volume"),
      
      radioButtons("modelo_estvol",
                   label = "Selecione o modelo para ser utilizado:",
                   choices = c(
                     "LN(VFFC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e",
                     "VFFC = b0 + b1 * DAP + b2 * HT + e",
                     "LN(VFFC) = b0 + b1 * 1/DAP + e",
                     "VFFC = b0 + b1 * DAP + e", 
                     "VFFC = b0 + b1 * DAP² + e", 
                     "VFFC = b0 + b1 * DAP + b2 * DAP² + e",
                     "VFFC = b0 + b1 * LN(DAP) + e"
                   ) ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'DAP_estvol', # Id
        "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = DAP_names,
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'# ,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )
      
      
      
      
    )
    
    
  })
  output$ui_estvol2 <- renderUI({
    
    # Precisa ter Altura no modelo
    validate(need(grepl( "\\<HT\\>",input$modelo_estvol), "" ) )  
    
    data <- rawData()
    
 list(
   
   selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
     'HT_estvol', # Id
     "Selecione a coluna da altura (m):", # nome que sera mostrado na UI
     choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
     selected = HT_names,
     multiple = T,
     options = list(
       maxItems = 1,
       placeholder = 'selecione uma coluna abaixo'#,
       # onInitialize = I('function() { this.setValue(""); }')
     ) # options
   )
   
 )    
    
  })
  output$ui_estvol3 <- renderUI({
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'bo_estvol', # Id
        "Insira o valor para o b0:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      ),
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b1_estvol', # Id
        "Insira o valor para o b1:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      )
      
      
    )
    
  })
  output$ui_estvol4 <- renderUI({
    
    # Precisa ter b2 no modelo
    validate(need(grepl( "\\<b2\\>",input$modelo_estvol), "" ) )  
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b2_estvol', # Id
        "Insira o valor para o b2:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
        )
      
    )
    
  })
  
  output$vol_est_table <- renderDataTable({
    
    est_voldt <- est_vol()
    
    datatable( as.data.frame(est_voldt),
               options = list(searching = T,
                              paging=T,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )  ) 
    
  }) 
  
      # Totalização de Parcelas ####
  
  # dados / funcao inv_summary
  newData <- reactive({
    
    validate(need(input$df == "Dados em nivel de arvore", "Base de dados incompativel" ),
             need(input$DAPnew!= "","Por favor selecione a coluna referente a 'dap' "),
             need(input$HTnew!= "","Por favor selecione a coluna referente a 'altura' "),
             need(input$VCCnew!= "","Por favor selecione a coluna referente a 'volume' "),
             need(input$area_parcelanew!= "","Por favor insira um valor ou selecione uma coluna referente a 'area da parcela' "),
             need(input$area_totalnew!= "","Por favor insira um valor ou selecione uma coluna  referente a 'area total' ")
             
    )
    
      #dados <- rawData()
      dados <- totData()
      
      x <- inv_summary(df           = dados, 
                       DAP          = input$DAPnew, 
                       HT           = input$HTnew,
                       VCC          = input$VCCnew,
                       area_parcela = input$area_parcelanew,
                       groups       = input$gruposnew,
                       area_total   = input$area_totalnew,
                       idade        = input$idadenew,
                       VSC          = input$VSCnew,
                       Hd           = input$Hdnew)
      
      x
      
  })
  
  # UI
  output$tot_parc_ui1 <- renderUI({
    
    #data <- rawData()
    data <- totData()
    
    list(
    
    h3("Totalização de Parcelas"),
      
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'DAPnew', # Id
      "Selecione a coluna do DAP (cm):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'# ,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    ),
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'HTnew', # Id
      "Selecione a coluna da altura (m):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = HT_names,
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        # onInitialize = I('function() { this.setValue(""); }')
      ) # options
    ),

    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      'VCCnew', # Id
      "Selecione a coluna do volume com casca (m³):", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VCC_names,
      multiple = T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options
    )
    
   
    
    )
    
  })
 
  output$tot_parc_ui2 <- renderUI({
    
   # data <- rawData()
    data <- totData()
    
    list(


      switch(input$area_radio_new,
             "Manualmente" =  numericInput("area_parcelanew", 
                                           label = "Insira o valor da área da parcela (m²):",
                                           value = 10000),
             
             "Lista de colunas" = selectizeInput("area_parcelanew",
                                                 label = "Selecione a coluna da área da parcela (m²):",
                                                 choices = names(data),
                                                 selected = area_parcela_names,
                                                 multiple = T,
                                                 options = list(
                                                   maxItems = 1,
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                               #    onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      switch(input$area_radio_new,
             "Manualmente" =  numericInput("area_totalnew", 
                                           label = "Insira o valor da área total (ha):",
                                           value = 50),
             
             "Lista de colunas" = selectizeInput("area_totalnew",
                                                 label = "Selecione a coluna da área total (ha)",
                                                 choices = names(data),
                                                 selected = area_total_names,
                                                 multiple = T,
                                                 options = list(
                                                   maxItems = 1,
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                 #  onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),

      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposnew', # Id
        "selecione as variáveis pivô:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,
        selected = grupos_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo'#,
          #onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      
      h3("Variaveis opcionais:"),
      
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadenew', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = idade_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VSCnew', # Id
        "selecione a coluna do volume sem casca (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        # selected = VSC_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'Hdnew', # Id
        "Selecione a coluna da altura dominante (m):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        # selected = HD_names,
        options = list(
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )    
      
      
    )
    
    
  })

  # tabela
  output$newdata <- renderDataTable({ # renderizamos uma DT::DataTable
    
    data <- newData() 
    
      datatable(data,
                options = list(initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                 "}")
                )   
      ) # Criamos uma DT::datatable com base no objeto

  })

  
  # Dado utilizado no inventario ####
  
  # switch que muda o dado a ser utilizado
  invData <- reactive({
    
    if(is.null(input$df)){ return()}
    
    # Se o dado for em nivel de arvore, a totalização de parcelas deve ser feita para que
    # NewData possa ser inserido em acs. Sem essa condição a ui gera mensagens de erro
    switch(input$df, 
           "Dados em nivel de arvore" = if(is.null(input$VCCnew) ){return()}else{ newData()},
           "Dados em nivel de parcela" = rawData() )
    
  })
  
  # ACS ####
  
  # funcao acs aplicada em invData
  tabacs <- reactive({
    
    validate(need(input$VCCacs!= "","Por favor selecione a coluna referente a 'volume' "),
             need(input$area_parcelaacs!= "","Por favor insira um valor ou selecione uma coluna referente a 'area da parcela' "),
             need(input$area_totalacs!= "","Por favor insira um valor ou selecione uma coluna  referente a 'area total' ")
             
    )
      dados <- invData()
      
      x <-     acs(df             = dados,
                   VCC            = input$VCCacs,
                   area_parcela   = input$area_parcelaacs,
                   area_total     = input$area_totalacs, 
                   idade          = input$idadeacs,
                   grupos         = input$gruposacs, 
                   alpha          = input$alphaacs, 
                   Erro           = input$erroacs, 
                   casas_decimais = input$cdacs, 
                   pop            = input$popacs, 
                   tidy           = input$tidyacs)
      
      x
    
  })
  
  # UI: as opcoes (choices) sao os nomes de invData
  output$acs_ui1 <- renderUI({
    
    data <- invData()
    
    list(
      
      h3("Amostragem Casual Simples"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VCCacs', # Id
        "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = VCC_names, 
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )
      
    )
  })
  output$acs_ui2 <- renderUI({
    
    data <- invData()
    
    list(
      
      switch(input$area_radio_acs,
             "Manualmente" =  numericInput("area_parcelaacs", 
                                           label = "Insira o valor da área da parcela (m²):",
                                           value = 810),
             
             "Lista de colunas" = selectizeInput("area_parcelaacs",
                                                 label = "Selecione a coluna da área da parcela (m²):",
                                                 choices = names(data),
                                                 selected = area_parcela_names,     
                                                 multiple = T,
                                                 options = list(
                                                   maxItems = 1,
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      switch(input$area_radio_acs,
             "Manualmente" =  numericInput("area_totalacs", 
                                           label = "Insira o valor da área total (ha):",
                                           value = 45),
             
             "Lista de colunas" = selectizeInput("area_totalacs",
                                                 label = "Selecione a coluna da área total (ha):",
                                                 choices = names(data),
                                                 selected = area_total_names,     
                                                 multiple = T,
                                                 options = list(
                                                   maxItems = 1,
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #  onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      h4("Variaveis opcionais:"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadeacs', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), 
        selected = idade_names,     
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposacs', # Id
        "Selecione as variáveis pivô:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,  # permite mais de uma opcao ser selecionada
        selected = NULL,     
        options = list(
          placeholder = 'Selecione as variaveis abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      sliderInput("erroacs", 
                  label = "Selecione o erro admitido (%):", 
                  min = 1, 
                  max = 20, 
                  value = 10,
                  step = 1),
      
      sliderInput("alphaacs", 
                  label = "Selecione o nível de significância:", 
                  min = 0.01, 
                  max = 0.10, 
                  value = 0.05,
                  step = 0.01),
      
      sliderInput("cdacs", 
                  label = "Selecione o nº de casas decimais:", 
                  min = 0, 
                  max = 10, 
                  value = 4,
                  step = 1),
      
      radioButtons(
        inputId='popacs', # Id
        label='Considerar a população infinita ou finita?', # nome que sera mostrado na UI
        choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
        selected="inf"
      ),
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId="tidyacs",  #Id
        label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
        choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
        selected=T) # valor que sera selecionado inicialmente
      
    )
  })
  
  # tabela
  output$acs <- renderDataTable({
    
    acsdt <- tabacs() 
      # converte em datatable        # cria formattable
      as.datatable( formattable(acsdt, 
                                list(    # colore a linha 6 da coluna dois de verde ou vemelho, se ela for menor ou maior que o numero da linha 1 coluna 2
                                  area(row=6, col=2) ~  formatter("span", 
                                  style = x ~ formattable::style(color = ifelse(x <= acsdt[1,2], "#108e00", "red"))) ,
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=10, col=2) ~ formatter("span", 
                                  style = x ~ formattable::style(color = ifelse(x <= input$erroacs, "#108e00", "red")))
                                  
                                  
                                )#list
      ), #formattable
      # pre seleciona linhas
      selection = list(mode = 'multiple', selected = c(6,10,15,16), target = 'row'),
      options = list(searching = FALSE,
                     paging=FALSE,
                     initComplete = JS( # muda cor do cabecalho
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                       "}")
      ) 
      
      
      
      ) #as.datatable
      
      

  })
  
  # ACE ####
  
  # resultado 1 da funcao ace aplicada em invData
  tabace1 <- reactive({
    
    validate(need(input$VCCace!= "","Por favor selecione a coluna referente a 'volume' "),
             need(input$area_parcelaace!= "","Por favor insira um valor ou selecione uma coluna referente a 'area da parcela' "),
             need(input$area_estratoace!= "","Por favor insira um valor ou selecione uma coluna  referente a 'area total' "),
             need(input$gruposace!= "","Por favor insira um valor ou selecione uma coluna  referente a 'variáveis pivô' ")
     )
      dados <- invData()
      
      x <- ace(df             = dados, 
               VCC            = input$VCCace, 
               area_parcela   = input$area_parcelaace, 
               area_estrato   = input$area_estratoace, 
               grupos         = input$gruposace, 
               idade          = input$idadeace, 
               alpha          = input$alphaace, 
               Erro           = input$erroace, 
               casas_decimais = input$cdace, 
               pop            = input$popace, 
               tidy           = input$tidyace)[[1]]
      x

  })
  
  # resultado 2 da funcao ace aplicada em invData
  tabace2 <- reactive({
    
    validate(need(input$VCCace!= "","Por favor selecione a coluna referente a 'volume' "),
             need(input$area_parcelaace!= "","Por favor insira um valor ou selecione uma coluna referente a 'area da parcela' "),
             need(input$area_estratoace!= "","Por favor insira um valor ou selecione uma coluna  referente a 'area total' "),
             need(input$gruposace!= "","Por favor insira um valor referente a 'variáveis pivô' ")
    )
    
      dados <- invData()
      
      x <- ace(df = dados, 
               VCC            = input$VCCace, 
               area_parcela   = input$area_parcelaace , 
               area_estrato   = input$area_estratoace, 
               grupos         = input$gruposace, 
               idade          = input$idadeace, 
               alpha          = input$alphaace, 
               Erro           = input$erroace, 
               casas_decimais = input$cdace, 
               pop            = input$popace, 
               tidy           = input$tidyace)[[2]]
      
      x

  })
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  output$ace_ui <- renderUI({
    
    data <- invData()
    
    list(
      
      h3("Amostragem Casual Estratificada"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VCCace', # Id
        "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = VCC_names,     
        multiple = T,
        options = list(
        maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'area_parcelaace', # Id
        "Selecione a coluna da área da parcela (m²):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = area_parcela_names,     
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'area_estratoace', # Id
        "Selecione a coluna da área total (ha):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = area_total_names,     
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposace', # Id
        "Selecione a(s) coluna(s) para estratificação:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,  # permite mais de uma opcao ser selecionada
        selected = estratos_names,     
        options = list(
          maxItems = 1,
          placeholder = 'Selecione as variaveis abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      h4("Variaveis opcionais:"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadeace', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = idade_names,     
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          #onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      
      sliderInput("erroace", 
                  label = "Selecione o erro admitido (%):", 
                  min = 1, 
                  max = 20, 
                  value = 10,
                  step = 1),
      
      sliderInput("alphaace", 
                  label = "Selecione o nível de significância:", 
                  min = 0.01, 
                  max = 0.10, 
                  value = 0.05,
                  step = 0.01),
      
      sliderInput("cdace", 
                  label = "Selecione o nº de casas decimais:", 
                  min = 0, 
                  max = 10, 
                  value = 4,
                  step = 1),
      
      radioButtons(
        inputId='popace', # Id
        label='Considerar a população infinita ou finita?', # nome que sera mostrado na UI
        choices=c(Infinita="inf", Finita="fin"), # opcoes e seus nomes
        selected="inf"
      ),
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId="tidyace",  #Id
        label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
        choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
        selected=T) # valor que sera selecionado inicialmente
      
    )
  }) 
  
  # tabela ace1
  output$ace1 <- renderDataTable({
    
    ace1dt <- tabace1() 
    
      datatable( ace1dt, # seleciona a linha 5 previamente
                 selection = list(mode = 'multiple', selected = c(13,17,18,19), target = 'row'),
                 options = list(searching = FALSE,
                                paging=FALSE,
                                initComplete = JS( # muda a cor do cabecalho
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                  "}")
                 )   
                 
      )

  })
  
  # tabela ace2
  output$ace2 <- renderDataTable({
    
    ace2dt <- tabace2() 
    
      # converte em datatable        # cria formattable
      as.datatable( formattable(ace2dt, 
                                list(
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=5, col=2) ~ formatter("span", 
                                  style = x ~ formattable::style(color = ifelse(x <= input$erroace, "#108e00", "red")))
                                  
                                  
                                )#list
      ), #formattable
      # pre seleciona linhas
      selection = list(mode = 'multiple', selected = c(5,10,11), target = 'row'),
      options = list(searching = FALSE,
                     paging=FALSE,
                     initComplete = JS( # muda cor do cabecalho
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                       "}")
      ) 
      
      
      
      )
      

  })
  
  # AS ####
  
  # funcao as aplicado em invData
  tabas <- reactive({
    
    validate(need(input$VCCas!= "","Por favor selecione a coluna referente a 'volume' "),
             need(input$area_parcelaas!= "","Por favor insira um valor ou selecione uma coluna referente a 'area da parcela' "),
             need(input$area_totalas!= "","Por favor insira um valor ou selecione uma coluna  referente a 'area total' ")
    )
    
    dados <- invData()
      
      x <- as_diffs(df             = dados, 
                    VCC            = input$VCCas,
                    area_parcela   = input$area_parcelaas ,
                    area_total     = input$area_totalas,
                    idade          = input$idadeas,
                    grupos         = input$gruposas,
                    alpha          = input$alphaas,
                    Erro           = input$erroas,
                    casas_decimais = input$cdas,
                    tidy           = input$tidyas)
      
      x

  }) 
  
  # UI: as opcoes (choices) sao os nomes de invData
  
  output$as_ui1 <- renderUI({
    
    data <- invData()
    
    list(
      
      h3("Amostragem Sistematica"),
      
      helpText("Utiliza-se o método das diferenças sucessivas, portanto, assume-se que os dados estão organizados de forma ordenada."),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'VCCas', # Id
        "Selecione a coluna do volume (m³):", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = VCC_names, 
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          # onInitialize = I('function() { this.setValue(""); }')
        ) # options
      )
      
    )
  })
  output$as_ui2 <- renderUI({
    
    data <- invData()
    
    list(
      
      
      switch(input$area_radio_as,
             "Manualmente" =  numericInput("area_parcelaas", 
                                           label = "Insira o valor da área da parcela (m²):",
                                           value = 810),
             
             "Lista de colunas" = selectizeInput("area_parcelaas",
                                                 label = "Selecione a coluna da área da parcela (m²):",
                                                 choices = names(data),
                                                 selected = area_parcela_names,     
                                                 multiple = T,
                                                 options = list(
                                                   maxItems = 1,
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      switch(input$area_radio_as,
             "Manualmente" =  numericInput("area_totalas", 
                                           label = "Insira o valor da área total (ha):",
                                           value = 45),
             
             "Lista de colunas" = selectizeInput("area_totalas",
                                                 label = "Selecione a coluna da área total (ha):",
                                                 choices = names(data),
                                                 selected = area_total_names,     
                                                 multiple = T,
                                                 options = list(
                                                   maxItems = 1,
                                                   placeholder = 'Selecione uma coluna abaixo:'#,
                                                   #  onInitialize = I('function() { this.setValue(""); }')
                                                 ) # options    
             )# selectize
      ),
      
      
      h4("Variaveis opcionais:"),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'idadeas', # Id
        "Selecione a coluna da idade:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        selected = idade_names,     
        multiple = T,
        options = list(
          maxItems = 1,
          placeholder = 'selecione uma coluna abaixo'#,
          #onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
        'gruposas', # Id
        "Selecione as variáveis pivô:", # nome que sera mostrado na UI
        choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
        multiple = TRUE,  # permite mais de uma opcao ser selecionada
        options = list(
          maxItems = 1,
          placeholder = 'Selecione as variaveis abaixo',
          onInitialize = I('function() { this.setValue(""); }')
        ) # options
      ),
      
      sliderInput("erroas", 
                  label = "Selecione o erro admitido (%):", 
                  min = 1, 
                  max = 20, 
                  value = 10,
                  step = 1),
      
      sliderInput("alphaas", 
                  label = "Selecione o nível de significância:", 
                  min = 0.01, 
                  max = 0.10, 
                  value = 0.05,
                  step = 0.01),
      
      sliderInput("cdas", 
                  label = "Selecione o nº de casas decimais:", 
                  min = 0, 
                  max = 10, 
                  value = 4,
                  step = 1),
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId="tidyas",  #Id
        label='Selecione o arranjo da tabela:', # nome que sera mostrado na UI
        choices=c(Vertical = T, Horizontal = F), # opcoes e seus nomes
        selected=T)
      
    )
  })
  
  # tabela as
  output$as <- renderDataTable({
    
    asdt <- tabas() 
    
      # converte em datatable        # cria formattable
      as.datatable( formattable(asdt, 
                                list(    # colore a linha 6 da coluna dois de verde ou vemelho, se ela for menor ou maior que o numero da linha 1 coluna 2
                                  area(row=6, col=2) ~  formatter("span", 
                                  style = x ~ formattable::style(color = ifelse(x <= asdt[1,2], "#108e00", "red"))) ,
                                  # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                  area(row=10, col=2) ~ formatter("span", 
                                  style = x ~ formattable::style(color = ifelse(x <= input$erroas, "#108e00", "red")))
                                  
                                  
                                )#list
      ), #formattable
      # pre seleciona linhas
      selection = list(mode = 'multiple', selected = c(6,10,15,16), target = 'row'),
      options = list(searching = FALSE,
                     paging=FALSE,
                     initComplete = JS( # muda cor do cabecalho
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                       "}")
      ) 
      
      
      
      )

  })
  
  
  
  # Download tabelas ####
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Dado utilizado / filtrado"         = rawData(),
           "Indice diversidade"                = tabdiversidade(),
           "Matriz similaridade - Jaccard"     = tibble::rownames_to_column(tabmsimilaridade1(), " "),
           "Matriz similaridade - Sorensen"    = tibble::rownames_to_column(tabmsimilaridade2(), " "),
           "Pareado similaridade"              = tabpsimilaridade(),
           "Indice de agregacao"               = tabagregate(),
           "Estrutura"                         = tabestrutura(),
           "BDq Meyer"                         = tabBDq1(),
           "BDq Meyer - Coeficientes"          = tabBDq3(),
           "Estimacao de volume"               = totData(),
           "Totalizacao de parcelas"           = newData(),
           "Amostragem Casual Simples"         = tabacs(),
           "Amostragem Casual Estratificada 1" = tabace2(),
           "Amostragem Casual Estratificada 2" = tabace1(),
           "Amostragem Sistematica"            = tabas()
           )
  })
  
  output$table <- renderDataTable({
    
    datadownload <- datasetInput()
    
    datatable( datadownload,
               options = list(searching = FALSE,
                              paging=T,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
                              
               )  )
    
  }) 
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      
      if(input$datasetformat==".csv")
      {
        paste(input$dataset, '.csv', sep='') 
      }
      else if(input$datasetformat==".xlsx")
      {
        paste(input$dataset, '.xlsx', sep='') 
      }
    },
    
    content = function(file) {
      if(input$datasetformat==".csv")
      {
        write.csv2(datasetInput(), file, row.names = F)
      }
      else if(input$datasetformat==".xlsx")
      {
       xlsx::write.xlsx2(as.data.frame( datasetInput() ), file, row.names = F)
      }
      
      
      
    }
  )
  
  # Download graficos ####
  
  graphInput <- reactive({
    switch(input$graph_d,
           "Dendrograma - Jaccard"     = msim1_graph(),
           "Dendrograma - Sorensen"    = msim2_graph(),
           "Distribuicao - BDq Meyer"  = BDq_graph() )
  })
  
  output$graph_d_out <- renderPlot({
    
    g <- graphInput()
    
    g

    
  }) 
  
  output$downloadGraph <- downloadHandler(
    filename = function() { 
      
      if(input$graphformat==".png")
      {
        paste(input$graph_d, '.png', sep='') 
      }
      else if(input$graphformat==".jpg")
      {
        paste(input$graph_d, '.jpg', sep='') 
      }
      else if(input$graphformat==".pdf")
      {
        paste(input$graph_d, '.pdf', sep='') 
      }
      
    },
    
    content = function(file) {

        ggsave(file, graphInput(), width = 12, height = 6 )

      
    }
  )
  
  
})
