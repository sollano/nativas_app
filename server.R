options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize=30*1024^2) 
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


# Data e functions ####

ex_fuste <- read.csv2("examples/Inventory_exemplo_fuste.csv",fileEncoding="UTF-8")
ex_arvore <- read.csv("examples/Inventory_exemplo_arvore.csv",fileEncoding="UTF-8")
#ex <- read.csv("examples/Inventory_exemplo_2.csv",fileEncoding="UTF-8")

source("funs/check_names.R"        , encoding="UTF-8")
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
source("funs/tree_summarise.R"     , encoding="UTF-8")
source("funs/round_df.R"           , encoding="UTF-8")
source("funs/estrat_vert_souza.R"  , encoding="UTF-8")
source("funs/classe_diametro.R"    , encoding="UTF-8")
source("funs/htdapratio.R"         , encoding="UTF-8")
source("funs/consistency.R"        , encoding="UTF-8")
source("funs/xlsx.write.list.R"    , encoding="UTF-8")
source("funs/check_numeric.R"      , encoding="UTF-8")
source("funs/notin.R"              , encoding="UTF-8")
source("funs/hdjoin.R"             , encoding="UTF-8")
source("funs/check_dap_min.R"      , encoding="UTF-8")
source("funs/check_yi.R"           , encoding="UTF-8")
source("funs/alt.filter.keep.R"    , encoding="UTF-8")
source("funs/alt.filter.rm.R"      , encoding="UTF-8")
source("funs/renamer.R"            , encoding="UTF-8")

# vectors for names ####

arvore_names <- c("ARVORE", "Arvore", "arvore", "ARV", "Arv", "arv", "ARV.", "Arv.", "arv.","NP","Np","np","Árvore","ÁRVORE","árvore" )
especies_names <- c("nome.cient","scientific.name","Scientific.Name","SCIENTIFIC.NAME" ,"scientific_name", "Scientific_Name","SCIENTIFIC_NAME","nome.cientifico", "Nome.Cientifico","NOME.CIENTIFICO","nome_cientifico", "Nome_Cientifico","NOME_CIENTIFICO","nome.cientifíco", "Nome.Científico","NOME.CIENTÍFICO","nome_científico", "Nome_Científico","NOME_CIENTÍFICO","nome cientifico", "Nome Cientifico","NOME CIENTIFICO","nome científico", "Nome Científico","NOME CIENTÍFICO","Especie", "especie", "Especies", "especies","Espécie", "espécie", "Espécies", "espécies")
parcelas_names <- c("transecto","transect", "Transect", "TRNASECT", "transect.code","Transect.Code","TRANSECT.CODE","transect_code","Transect_Code","TRANSECT_CODE","parcela", "Parcela","PARCELA","cod.parcela","Cod.Parcela","COD.PARCELA", "cod_parcela","Cod_Parcela","COD_PARCELA")
est.vertical_names <- c("pos.copa","canopy", "canopy_09")
est.interno_names <- c("luminosidade","light", "light_09")

CAP_names <- c("CAP","Cap","cap", "cbh", "Cbh","CBH","CBH_11","CAP(cm)","CAP(cm)","Cap (cm)","Cap(cm)")
DAP_names <- c("DAP","Dap","dap", "dbh", "Dbh","DBH","DBH_11","DAP(cm)","DAP(cm)","Dap (cm)","Dap(cm)")
HT_names <- c("HT_EST", "HT", "Ht", "ht","Htot","ALTURA","Altura","Altura_Total", "ALTURA_TOTAL","HT (m)","HT(m)","Ht (m)","Ht(m)","Altura Total (m)","Altura total(m)","Altura (m)","Altura(m)", "ALTURA (m)", "ALTURA(m)")
VCC_names <- c("VCC","Vcc", "vcc", "VOL", "Vol", "vol" ,"VOLUME", "Volume (m³)", "VOLUME (m³)", "VOL(m³)", "Volume(m³)", "VOLUME(m³)", "VOL(m³)")
area_parcela_names <- c("trans.area","AREA_PARCELA","Area_Parcela","area_parcela","parc.area" ,"AREAPARCELA", "areaparcela", "transect.area", "Transect.Area", "TRANSECT.AREA","transect_area","Transect_Area","TRANSECT_AREA")
area_total_names <- c("sub.area","AREA_TOTAL", "AREATOTAL", "area_total", "areatotal","AREA_TALHAO", "AREATALHAO", "area_talhao", "areatalhao","total.area","Total.Area","TOTAL.AREA","total_area","Total_Area","TOTAL_AREA", "area.total", "Area.total", "Area.Total", "AREA.TOTAL")
idade_names <- c("IDADE", "Idade","idade")
VSC_names <- c("VSC","Vsc", "vsc")
HD_names <- c("HD", "Hd", "hd", "ALTURA_DOMINANTE", "ALT_DOM")
grupos_names <- c(c("TALHAO", "PARCELA"), c("area.code", "transect"), c("codigo", "transecto"), "parcela", "PARCELA", "transect", "cod.parcela", "Cod.parcela", "COD.PARCELA")
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")

# Server ####

shinyServer(function(input, output, session) {

  
  # Importação ####
  
  #ui
  output$upload      <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload", "" )  )
    
    list(   
      
      radioButtons("df_extension", 
                   "Informe o formato do arquivo:", 
                   choices = c(".csv (Valor separado por virgulas) ou .txt (arquivo de texto)",
                               ".xlsx (Excel)"),
                   selected =".xlsx (Excel)")
    )
  })
  output$upload_csv  <- renderUI({
    
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
      
      #radioButtons(inputId = "mv_excel",label = "Valores ausentes", choices = c("Espaço vazio" = "", "NA" = "NA"), inline = T ),
      
      # input de arquivos
      fileInput( 
        inputId = "file2", # Id
        
        label = "Selecione o arquivo: (.xlsx)", # nome que sera mostrado na UI
        
        # So aceita .xlsx
        accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                 '.xlsx'))#,
      
      
      #div("Recomendamos o uso do formato .csv", style = "color:blue")
      
      
    )
    
    
  })
  
  #tabela
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    # sera vazio caso nao seja selecionado "fazer o upload"
    validate(
      need(input$df_select, ""),
      need(input$df_extension, ""),
      need(input$df_select == "Fazer o upload" , "" )  )
    
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
    
    # precisa do caminho do dado pra rodar os codigos a seguir
    req(inFile)
    
    if(input$df_extension != ".xlsx (Excel)")
    {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."))
      raw_data <-  readxl::read_xlsx(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n, na = c("","NA")) 
      #raw_data <-  openxlsx::read.xlsx(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n) 
      raw_data <- as.data.frame(raw_data)
    }
    
    raw_data # tabela final a ser mostrada. 
    
  })
  
  # rawData_ (com traco) sera o dado bruto sem filtro. Este dataframe sera utilizado em todo o app
  rawData_ <- reactive({
    
    # raw data, sera definido como o exemplo, ou o dado de upload, dependendo do usuario.
    # para evitar erros, caso seja selecionado "Fazer o upload" mas o dado ainda não tenha sido uploadado,
    # sera retornanado vazio
    switch(input$df_select, 
           "Fazer o upload" = if(is.null(input$file1) && is.null(input$file2)){return()}else{upData()},
           "Utilizar o dado de exemplo em nivel de fuste" = ex_fuste,
           "Utilizar o dado de exemplo em nivel de arvore" = ex_arvore )
    
  })
  
  # render table
  output$rawdata <- DT::renderDataTable({ # renderizamos uma DT::DataTable
    
    validate(need(!is.null(rawData_()), "Please import a dataset"))
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    data <- rawData_() 
    
    datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}"),
                pageLength = 25
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
  })
  
  # logging ####
  
  observe({
    fingerprint <- input$fingerprint
    ipid <- input$ipid
    #print(fingerprint)
    #print(ipid)
  })
  
  output$testtext <- renderText(paste("     fingerprint: ", input$fingerprint, "     ip: ", input$ipid))
  
  
  
  # ####
  
  # send data ####
  
  
  send_sheet <- reactive({
    
    validate(need( !is.null(upData()) , "" )  )
    
    #pegar os nomes
    varnames <- varnames()
    
    # Cria um dataframe com os nomes padronizados das variaveis mapeadas
    df_up <- renamer(upData(), arvore = varnames$arvore,
                     parcelas=varnames$parcelas,
                     especies=varnames$especies,
                     
                     cap = varnames$cap,
                     dap= varnames$dap,
                     ht= varnames$ht,
                     
                     vcc=varnames$vcc,
                     vsc=varnames$vsc,
                     area.parcela=varnames$area.parcela,
                     area.total=varnames$area.total,
                     
                     est.vertical=varnames$est.vertical,
                     est.interna=varnames$est.interna,
                     estrato=varnames$estrato )
    # Faz login na conta do google usando o token
    #suppressMessages(googlesheets::gs_auth(token = "googlesheets_token.rds",verbose = FALSE))
    
    # Manda o arquivo para a conta da google, no google spreadsheets
    #googlesheets::gs_new(title=paste(round(abs(rnorm(1,1,1)),2),"nat_app", Sys.Date(),format(Sys.time(), "%H_%M_%S"),sep = "_"),input = df_up,trim = FALSE,verbose = FALSE)

    #login
    suppressMessages(drive_auth("googlesheets_token.rds",verbose = F))
    #print("logged in")
    #nome do arquivo
    fn <-paste(Sys.Date(),format(Sys.time(),"%H_%M_%S"),round(abs(rnorm(1,1,1)),2),"nat_app",".csv",sep = "_")
    
    # salva arquivo temporario no disco
    write.csv(df_up,file = fn,row.names = FALSE)
    
    # manda pro drive
    suppressMessages(drive_upload(fn, paste("NativasApp",fn,sep="/"),verbose = F))
    #print("file uploaded")
    # delete arquivo temporario
    unlink(fn)
    
    # deleta objeto fn
    rm(fn)
    
    
    
  })
  
  # dummy observer for linux (makes session flush when a download is made)
  observe({
    invalidateLater(500)
  })  
  observe({
    #print(rnDownloads$ndown)
    # So rodar se algum dado for uploadado
    req( !is.null(upData()) )
    # Se algum botao de download for clicado, enviar dados para a nuvem
    req(rnDownloads$ndown>0)
    send_sheet()
  })
  
  # Mapeamento ####
  
  # ui
  output$selec_arvore       <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.arvore", # Id
      strong("Esta variável é necessária para o processamento de dados em nível de fuste"), # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = arvore_names,
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
  output$selec_parcelas     <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.parcelas", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = parcelas_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_especies     <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.especies", # Id
      NULL, # nome que sera mostrado na UI
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
  
  output$selec_cap          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.cap", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = CAP_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_dap          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dap", # Id
      strong("Caso o CAP seja fornecido, o DAP será calculado automaticamente"), # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_ht           <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.ht", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = HT_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$selec_vcc          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.vcc", # Id
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VCC_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_area.parcela <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.area.parcela",
                   "Pode ser informada como valor numérico na aba 'Preparação dos dados'", # nome que sera mostrado na UI
                   choices = names(data),
                   selected = area_parcela_names,
                   multiple = T,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  output$selec_area.total   <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.area.total",
                   strong("Áreas dos estratos devem ser inseridas aqui"), # nome que sera mostrado na UI
                   choices = names(data),
                   selected = area_total_names,
                   multiple = T,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  output$selec_vsc          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.vsc", # Id
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = VSC_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$selec_est.vertical_2 <- renderUI({
    
    data <- rawData_()
    
    switch(input$est.vert.calc,
           "Definir" =   h5("A estrutura vertical será calculada utilizando a variável altura, segundo o método de Souza (2002)."),
           
           "Inserir" =    selectizeInput("col.est.vertical",
                                         NULL, # nome que sera mostrado na UI
                                         choices = names(data),
                                         # selected =  ,
                                         multiple = T,
                                         options = list(
                                           maxItems = 1,
                                           placeholder = 'Selecione uma coluna abaixo:'#,
                                           #    onInitialize = I('function() { this.setValue(""); }')
                                         ) # options
           )
    ) #switch
    
  })
  output$selec_est.vertical_warning <- renderUI({
    
    req(input$est.vert.calc == "Definir" )
    validate(
      need(!is.null(input$col.ht) , # ht nao e nulo? quando a resposta for nao a mensagem aparece
           "Variável 'Altura' não definida. A estrutura vertical não será calculada." ), errorClass = "AVISO")
    
    
    
  })
  output$selec_est.interna  <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.est.interna",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                   # selected = ,
                   multiple = T,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  output$selec_estrato      <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.estrato",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                   selected = estratos_names,
                   multiple = T,
                   options = list(
                     maxItems = 10,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })

  # Preparação ####
  # ui
  output$selec_rotuloNI <- renderUI({
    
    validate(need(input$col.especies != "","") )
    
    data <- rawData_()
    
    list(
      
      h3("Espécie não-identificada"),
      
      selectizeInput("rotutuloNI",
                     "Selecione o(s) indice(s) referente(s) às espécies não identificadas:", # nome que sera mostrado na UI
                     choices = levels(as.factor(data[,input$col.especies])),
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Selecione um ou mais rótulos abaixo',
                       onInitialize = I('function() { this.setValue(""); }')
                     ) )
      
    )
    
  })
  output$rm_data_var <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.rm_data_var", # Id
      "Selecione a coluna que se deseja filtrar:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$rm_data_level <- renderUI({
    
    if( is.null(input$col.rm_data_var) || input$col.rm_data_var =="" ){
      
      opcoes <- NULL
      
    }else{
      
      data <- rawData_()
      
      opcoes <- levels(
        as.factor(
          data[,input$col.rm_data_var]))
    }
    
    list(
      
      selectizeInput("level.rm_data_level",
                     label = "Selecione o(s) nivel(s) que se deseja remover ou manter:",
                     choices = opcoes,
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Selecione o(s) nivel(s) abaixo',
                       onInitialize = I('function() { this.setValue(""); }')
                     ) # options    
      ),
      
      radioButtons("rm_or_keep",
                   label = "Remover, ou manter dados referentes ao nível selecionado?",
                   c("Remover"=FALSE, "Manter"=TRUE),
                   selected = TRUE,
                   inline = TRUE  )
      
    )
  })
  output$rm_vars <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.rm_vars", # Id
      "Selecione a(s) coluna(s) que se deseja remover:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(" "); }')
      ) # options    
    ) # selctize
    
    
  })
  # area numerico
  output$selec_area_parcela_num <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    req(is.null(input$col.area.parcela) || input$col.area.parcela=="" )
    
    list(
      
      h3("Área da parcela (m²) (numérico)*"),
      
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'num.area.parcela', # Id
        "Insira o valor para a Área da parcela:", # nome que sera mostrado na UI
        value = "", 
        step = 1
      )
      
    )
    
  })
  output$selec_area_total_num <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    req(is.null(input$col.area.total) || input$col.area.total=="" )
    
    list(
      h3("Área total (ha) (numérico)*"),
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'num.area.total', # Id
        "Insira o valor para a Área total:", # nome que sera mostrado na UI
        value = "", 
        step = 1
      )
      
    )
    
  })
  #UI estimar volume com casca 
  output$ui_estvcc1 <- renderUI({
    
    # Precisa que a tab de vcc seja selecionada
    # precisa que o usuario nao tenha selecionado o volume
    req(
      #input$est_ht_vol_tabset == "id_vcc", 
      is.null(input$col.vcc) || input$col.vcc =="" )
    
    list(
      
      h3("Estimaçao do volume com casca"),
      
      radioButtons("modelo_estvcc",
                   label = "Selecione o modelo para ser utilizado:",
                   choices = c(
                     "LN(VFCC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e",
                     "VFCC = b0 + b1^DAP + b2^HT + e",
                     "VFCC = b0 + b1 * DAP² + e",
                     "VFCC = b0 + b1 * DAP + b2 * DAP² + e",
                     "LN(VFCC) = b0 + b1 * DAP + b2 * DAP² + e",
                     "LN(VFCC) = b0 + b1 * LN(DAP) + e",
                     "LN(VFCC) = b0 + b1 * LN(DAP² * HT) + e",
                     "VFCC = b0 + b1 * DAP² * HT + e"
                   ),
                   inline=F
      )      )
    
    
    
  })
  output$ui_estvcc3 <- renderUI({
    
    # Precisa que a tab de vcc seja selecionada
    # precisa que o usuario nao tenha selecionado o volume
    req(
      #  input$est_ht_vol_tabset == "id_vcc",
      is.null(input$col.vcc) || input$col.vcc =="" )
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b0_estvcc', # Id
        "Insira o valor para o b0:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      ),
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b1_estvcc', # Id
        "Insira o valor para o b1:", # nome que sera mostrado na UI
        value = NULL, 
        step = 0.0001
      )
      
      
    )
    
  })
  output$ui_estvcc4 <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    # Precisa que a tab de vsc seja selecionada
    # Precisa ter b2 no modelo
    req(
      is.null(input$col.vcc) || input$col.vcc =="",
      grepl( "\\<b2\\>",input$modelo_estvcc)
    )
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b2_estvcc', # Id
        "Insira o valor para o b2:", # nome que sera mostrado na UI
        value = "", 
        step = 0.0001
      )
      
    )
    
  })
  
  # tabela
  # rawData sera o dado utilizado durante o resto do app
  # as alteracoes feitas em 'preparacao' serao salvas aqui
  # caso nao seja feito nada, rawData sera identico a rawData_
  rawData <- reactive({
   # send_sheet()
    data <- rawData_()
    nm <- varnames()
    
    # Antes de rodar as mensagens a seguir, um dado precisa ser importado
    validate(need(data,"please import a dataset"))
    
    # Check numeric para cap ou dap
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      validate(check_numeric(input$col.cap, data, "cap"))
    }else{
      validate(check_numeric(input$col.dap, data, "dap"))
    }
    
    # check numeric para ht
    validate(check_numeric(nm$ht, data, "ht"))
    
    # Aqui o dado nao ira rodar, caso essas condicoes sejam contrariadas
    # Elas serao mostradas em vermelho, devido a errorClass (definida no comeco da UI )
    #validate(
    #  need(is.numeric(data[[nm$dap]]), "dap column must be numeric"),
     # need(is.numeric(data[[nm$ht]]), "ht column must be numeric"), errorClass = "WRONG")
    
    # O if a seguir sera para calcular o DAP, caso o usuario insira a coluna CAP
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      data$DAP <- data[[nm$cap]]/pi
    }
    
    # Primeiro verificamos se o dap minimo iserido pelo usuario
    # nao ultrapassa os limites do dap fornecido
    if(nm$dap!=""){
    max.val <- max(data[[nm$dap]],na.rm=T)
    
    validate(check_dap_min(nm$diam.min,max.val)) 

    # caso nao ultrapasse, filtrar
    if(!is.na(nm$diam.min)){
      data <- data %>% dplyr::filter(is.na(.data[[nm$dap]]) | .data[[nm$dap]] >= nm$diam.min)
    }
    
    }
    # o proximo if sera para filtrar as linhas

    # se o usuario nao selecionar nada, retorna o dado normal 
    # (isso faz com o que o dado original seja exibido logo que se entra na aba de filtrar),
    # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis
    
    if( is.null(input$col.rm_data_var) || input$col.rm_data_var =="" || is.null(input$rm_or_keep) || input$rm_or_keep == ""){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
      
    }else{
      
      # Criar os grupos
      if( any(nm$estrato =="") ){grupos<-nm$parcela}else{grupos <- c(nm$estrato, nm$parcela)}
      
      
      if(input$rm_or_keep){ # mantem se for verdadeiro
        #boolean_vec <- data[[input$col.rm_data_var]]     %in%   input$level.rm_data_level
        data <- alt.filter.keep(df = data,var = input$col.rm_data_var, levelstokeep = input$level.rm_data_level, .groups = grupos)
      }else{                # remove se for falso
        #boolean_vec <- data[[input$col.rm_data_var]]   %notin%  input$level.rm_data_level
        data <- alt.filter.rm(df = data,var = input$col.rm_data_var, levelstorm = input$level.rm_data_level, .groups = grupos)
      }
      
      
      #data <- data[boolean_vec,]
      
      # data <- data %>% filter( ! .data[[input$col.rm_data_var]] %in% input$level.rm_data_level )
      
    }
    
  
    # A linha a seguir sera para remover uma ou mais colunas
    
    # se o usuario nao selecionar nada, uma coluna vazia e definida como nula,
    # ou seja, nao muda nada no dado.
    # por isso nao e necessario utilizar condicionais nesse caso
    
    data[, input$col.rm_vars] <- NULL
    
      # Converter zero em NA em dados numericos quando dado tiver mais de 1 linha
      if(nrow(data)>0){
        data <- data %>% dplyr::mutate_if(is.numeric, funs(dplyr::na_if(.,0)) ) 
      }

    # Volume com casca 
    
    # A seguir e feito o calculo do volume com casca, caso o usuario nao insira uma variavel de volume e as variaveis necessarias para o calculo
    
    # Modelos com b1 e apenas DAP
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) || input$modelo_estvcc =="" || nm$dap ==""  || input$b0_estvcc == "" || input$b1_estvcc == ""  ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
    }else{
      
      # Kopezi-Geharhardt
      if(input$modelo_estvcc == "VFCC = b0 + b1 * DAP² + e"){
        data$VCC <- input$b0_estvcc + input$b1_estvcc*data[[nm$dap]]^2
        data <- data %>% select(VCC, everything())
      }
      
      # Husch
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * LN(DAP) + e"){
        data$VCC <- exp( input$b0_estvcc + input$b1_estvcc*log(data[[nm$dap]]) )
        data <- data %>% select(VCC, everything())
      }
    }
    
    # Modelos com b1 b2 e apenas DAP
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) || is.null(input$b2_estvcc) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) || is.na(input$b2_estvcc) || input$modelo_estvcc =="" || nm$dap ==""  || input$b0_estvcc == "" || input$b1_estvcc == "" || input$b2_estvcc == "" ){
      
    }else{
      
      # Hohenadl-Krenn
      if(input$modelo_estvcc == "VFCC = b0 + b1 * DAP + b2 * DAP² + e"){
        data$VCC <- input$b0_estvcc + input$b1_estvcc*data[[nm$dap]] + input$b2_estvcc*data[[nm$dap]]^2 
        data <- data %>% select(VCC, everything())
      }
      # ?????
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * DAP + b2 * DAP² + e"){
        data$VCC <- exp(input$b0_estvcc + input$b1_estvcc*data[[nm$dap]] + input$b2_estvcc*data[[nm$dap]]^2)
        data <- data %>% select(VCC, everything())
      }
    }
    
    # Modelos com b1, DAP e HT
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) ||  is.null(input$col.ht) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) ||  is.na(input$col.ht) || input$modelo_estvcc =="" || nm$dap =="" || input$b0_estvcc == "" || input$b1_estvcc == ""  ){
      
    }else{
      
      # Spurr logaritimico
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * LN(DAP² * HT) + e"){
        data$VCC <- exp(input$b0_estvcc + input$b1_estvcc*log( (data[[nm$dap]]^2)*data[[input$col.ht]]  )  )
        data <- data %>% select(VCC, everything())
      }
      # Spurr
      if(input$modelo_estvcc == "VFCC = b0 + b1 * DAP² * HT + e"){
        data$VCC <- input$b0_estvcc + input$b1_estvcc*(data[[nm$dap]]^2)*data[[input$col.ht]]  
        data <- data %>% select(VCC, everything())
      }
    }
    
    # Modelos com b1, b2, DAP e HT
    if( is.null(input$modelo_estvcc) ||  is.null(nm$dap)  || is.null(input$b0_estvcc) || is.null(input$b1_estvcc) || is.null(input$b2_estvcc) ||  is.null(input$col.ht) || is.na(input$modelo_estvcc) ||  is.na(nm$dap)  || is.na(input$b0_estvcc) || is.na(input$b1_estvcc) || is.na(input$b2_estvcc) || is.na(input$col.ht) || input$modelo_estvcc =="" || nm$dap ==""  || input$b0_estvcc == "" || input$b1_estvcc == "" || input$b2_estvcc == "" || input$col.ht =="" ){
      
    }else{
      
      # Schumacher e Hall logaritimico
      if(input$modelo_estvcc == "LN(VFCC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e"){
        data$VCC <- exp(input$b0_estvcc + input$b1_estvcc*log(data[[nm$dap]]) + input$b2_estvcc*log(data[[input$col.ht]])  )
        data <- data %>% select(VCC, everything())
      }
      # Schumacher e Hall
      if(input$modelo_estvcc == "VFCC = b0 + b1^DAP + b2^HT + e"){
        data$VCC <- input$b0_estvcc + input$b1_estvcc^data[[nm$dap]] + input$b2_estvcc^data[[input$col.ht]]    
        data <- data %>% select(VCC, everything())
      }
    }
    
    
    
    # A seguir e feito o calculo da estrutura vertical, caso o usuario nao tenha inserido uma variavel referente a mesma, e selecione que desja calcular
    if(!is.null(input$est.vert.calc) && !is.na(input$est.vert.calc) && input$est.vert.calc=="Definir" && !is.null(input$col.ht) && !is.na(input$col.ht) ){
      
      data <- estrat_vert_souza(data, input$col.ht)
      
    }
    
    # O if a seguir sera para remover linhas inconsistentes selecionadas pelo usuario
    
    # se o usuario nao selecionar nada, nada acontece
    # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis
    
    if( ( is.null(input$consist_table_rows_selected) || input$consist_table_rows_selected == 0 || is.null(input$do_consist) || is.na(input$do_consist) || input$do_consist == "Nao" ) ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
      
    }else{
      data_inconsist <- consist_fun()
      
      # Pega o numero da linha original (rowid) das linhas que o usuario selecionou na tabela (input$consist_table_rows_selected)
      insconsist_rows <- data_inconsist [input$consist_table_rows_selected, "rowid" ]
      
      # remove linhas inconsistentes
      data <- data[ -insconsist_rows ,  ]
    }
    
    data <- as.data.frame(data)
    
    
  })
  
 # render
  output$prep_table <- DT::renderDataTable({
    
    validate(need(rawData(), "Please import a dataset"))
    
    data <- round_df(suppressWarnings(rawData()), 7)
    
    
    datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}"),
                pageLength = 25
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    
  })
  output$avisos_prep <- renderUI({
    data <- rawData_()
    nm <- varnames()

    # Essa parte do server ira gerar uma UI vazia, que gera avisos caso alguma condicao abaixo seja violada.
    #
    
    #Avisa quando o usuário remove todas as linhas do dado
    validate(
      need(nrow(rawData())>0,
           "Base de dados vazia"),
      errorClass = "AVISO"
    )
    # Os erros abaixo so poderao ser mostrados se o usuario selecionar alguma coluna para ser removido
    req(input$col.rm_vars)
   
    # A seguir sao geradas uma mensagem de aviso para cada uma das variaveis que o usuario pode selecionar na aba
    # de mapeamento, caso elas tambem sejam selecionadas para serem removidas.
    # E utilizado %in% pois input$col.rm_vars pode ter mais de um nome (o usuario pode remover mais de uma variavel de uma vez)
    # e utilizado ! pois a condicao necessaria (que nao gera aviso) e que a variavel nao seja removida.
    # A cor da mensagem (laranja) e definada no argumento errorClass
    validate(
      need(! nm$especies %in% input$col.rm_vars, 
           "You just removed the 'especies' variable. This will prevent you from running most of the app's functions") ,
      need(! nm$parcelas %in% input$col.rm_vars, 
           "You just removed the 'parcelas' variable. This will prevent you from running most of the app's functions") ,
      need(! nm$dap %in% input$col.rm_vars, 
           "You just removed the 'dap' variable. This will prevent you from running some of the app's functions") , 
      need(! nm$ht %in% input$col.rm_vars, 
           "You just removed the 'ht' variable. This will prevent you from running some of the app's functions") , 
      need(! nm$vcc %in% input$col.rm_vars, 
           "You just removed the 'vcc' variable. This will prevent you from running some of the app's functions") ,
      need(! nm$area.parcela %in% input$col.rm_vars, 
           "You just removed the 'area.parcela' variable. This will prevent you from running some of the app's functions"),
      need(! nm$area.total %in% input$col.rm_vars, 
           "You just removed the 'area.total' variable. This will prevent you from running some of the app's functions"), 
      need(! nm$estrato %in% input$col.rm_vars, 
           "You just removed the 'estrato' variable. This will prevent you from running some of the app's functions"),
      need(! nm$est.vertical %in% input$col.rm_vars, 
           "You just removed the 'est.vertical' variable. This will prevent you from running some of the app's functions"),
      need(! nm$est.interna %in% input$col.rm_vars, 
           "You just removed the 'est.interna' variable. This will prevent you from running some of the app's functions"), errorClass = "AVISO")
    
    # A errorClass AVISO foi criada no comeco da UI
    
  })
  # Set names ####
  varnames <- reactive({
    
    varnameslist <- list(
      
      arvore = input$col.arvore,
      parcelas=input$col.parcelas,
      especies=input$col.especies,
      
      cap = input$col.cap,
      dap=input$col.dap,
      ht=input$col.ht,
      
      vcc=input$col.vcc,
     # vsc=input$col.vsc,
      area.parcela=input$col.area.parcela,
      area.total=input$col.area.total,

     est.vertical=input$col.est.vertical,
     est.interna=input$col.est.interna,
     estrato=input$col.estrato,
     
     NI=input$rotutuloNI,
     IC=input$int.classe,
     diam.min=input$diam.min
      )
    
    # Se o usuario inserir valores numericos para as areas, defini-las na lista
    if(is.null(input$num.area.parcela)|| is.na(input$num.area.parcela) ||input$num.area.parcela==""){}else{varnameslist$area.parcela <- input$num.area.parcela  }
    if(is.null(input$num.area.total) || is.na(input$num.area.total) ||input$num.area.total==""){}else{varnameslist$area.total <- input$num.area.total  }
    
    # Se o usuario inserir valores de coeficientes, definir o nome de vcc como VCC
    # pois este sera calculado na aba preparacao
    if( !is.null(input$b0_estvcc) && !is.na(input$b0_estvcc) && !is.null(input$b1_estvcc) && !is.na(input$b1_estvcc)  ){
      varnameslist$vcc <- "VCC"
      }
    # se est vertical nao for nulo, altura nao for nula e o usuario quiser definir a est vertical, alterar o nome para est.vert
    # pois esta sera definida na aba preparacao
    if(!is.null(input$est.vert.calc) && !is.na(input$est.vert.calc) && input$est.vert.calc=="Definir" && !is.null(input$col.ht) && !is.na(input$col.ht) ){
      varnameslist$est.vertical <- "est.vert"
      }
    
    # se cao for selecionado, definir o nome de DAP para DAP, pois este sera calculado
    # na preparacao
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      varnameslist$dap <- "DAP"
    }
    
    
    # Os nomes nao selecionados serao salvos como NULL na lista,
    # estes sao entao convertidos para "", por conveniencia 
    #x <- data.frame(do.call(cbind, lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )  ))    

    x <- lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )   
    
    x
  })
  
  output$teste <- renderTable({
    varnames()
    
  })
  
  # Consistencia ####
  consist_fun <- reactive({
    
    data <- rawData_()

    # Aqui a funcao nao ira rodar, caso essas condicoes sejam contrariadas
    #  req(data, is.numeric(data[[input$col.dap]]),is.numeric(data[[input$col.ht]]) )
    
    # Check numeric para cap ou dap
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      req(input$col.cap)
      validate(check_numeric(input$col.cap, data, "cap"))
    }else{
      req(input$col.dap)
  
      validate(check_numeric(input$col.dap, data, "dap"))
    }

    validate(
      check_numeric(input$col.ht, data, "ht")  )
    
    #htdapratio(data, dap = input$col.dap, ht = input$col.ht) 
    suppressWarnings(
      consistency(
      df = data, 
      cap = input$col.cap, 
      dap = input$col.dap , 
      ht = input$col.ht, 
      parcela = input$col.parcelas, 
      especie = input$col.especies,
      arvore = input$col.arvore
      )) 
  })
  output$consist_warning1 <- renderUI({
    req(input$run_consist==TRUE)
    # Essa aviso ira aparcer na UI caso consit_fun() nao seja nulo.
    # Esse objeto so nao sera nulo quando a funcao rodar, ou seja,
    # quando houverem dados inconsistentes.
    # Caso contrario a UI fica vazia, e nao aparece nada
    validate(need(is.null(consist_fun()), "Dados inconsistentes foram detectados" ), errorClass = "AVISO")
  })
  output$consist_warning2 <- renderUI({
    req(input$run_consist==TRUE)
    # Essa aviso ira aparcer na UI caso consit_fun() nao seja um objeto valido.
    # Esse objeto so  sera nulo quando a funcao rodar e gerar um resultado nulo.
    # Isso ocorre quando nao sao encontradas inconsistencias.
    # Caso contrario a UI fica vazia, e nao aparece nada
    validate(need(consist_fun(), "Não foram encontradas inconsistências" ) )
  })
  output$consist_choice <- renderUI({
    req(input$run_consist==TRUE)
    req(consist_fun())
    
    # Funcionando de forma semelhante a consist_warning,
    # se o objeto consist_fun() nao for nulo, ou seja,
    # se houverem dados a serem consistidos, essa UI ira aparecer, que da a ele a opcao de
    # remover ou nao as linhas da tabela em que ele clicou
    radioButtons("do_consist",
                 h4("Remover linhas selecionadas da tabela de dados inconsistentes?"), 
                 c("Sim","Nao"),
                 selected = "Nao",
                 inline = T)
    
  })
  output$consist_table_help <- renderUI({
    req(input$run_consist==TRUE)
    req(consist_fun())
    
    # Se houverem inconsistencias, essa UI ira aparecer, 
    # que gera um titulo e um texto de ajuda para a mesma
    
    list(
    #  h2("Dados inconsistentes:"),
      p("Analise os dados a seguir e clique nas linhas que desejar remover da analise."),
      p("Em seguida basta selecionar a opção 'Sim' àbaixo, e os dados serão removidos.")
      
    )
  })
  output$consist_table <- DT::renderDataTable({
    req(input$run_consist==TRUE)
    # Se o usuario quiser ver a tabela, e ela nao for nula,
    # nem a opcao de ver ela for nula, mostrar se nao, aviso
    validate(need(consist_fun(),""), errorClass = "AVISO" )
    
    #req(input$show_consist_table, input$show_consist_table == "Sim")
    
    consist_data <- round_df(consist_fun() , 2)
    
    datatable(consist_data,
              
              options = list(
   #             width = "200px",
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}")
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    
    
  })
  
  
  # tot arvore ####
  
  tot_arvoreData <- reactive({
    
    
    nm <- varnames()
    dados <- rawData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df == "Dados em nivel de fuste", "Base de dados incompativel" ),
      need(nm$arvore,"Por favor mapeie a coluna referente a 'Árvore'  "),
      need(nm$dap,"Por favor mapeie a coluna referente a 'CAP' ou 'DAP'  ") )
    
    # Unir grupos e remover grupos nao fornecidos
    groups <- c(nm$estrato, nm$parcelas, nm$especies)
    groups <- groups[groups != ""]
    
    # O if a seguir sera para remover o CAP, caso o usuario tenha informado o CAP
    # Isso pq o dap ja estara calculado, e ele que sera usado nos calculos.
    # Se o CAP nao for removido, sera mantido, e pode causar confusao para os usuarios.
    if(!is.null(input$col.cap) && !is.na(input$col.cap) ){
      dados[[nm$cap]] <- NULL
    }
    
    tree_summarise(
      df           = dados,
      tree         = nm$arvore,
      dbh          = nm$dap,
      .groups      = groups,
      vwb          = nm$vcc
    )
  
    
    
  })
  
  output$tot_fuste_tab <- DT::renderDataTable({
    
    tab <- round_df(tot_arvoreData() , 4)
    
    datatable( tab,
               options = list(searching = FALSE,
                              paging=TRUE,
                              ordering=TRUE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )   
    ) 
    
  }) 
  
  arvData <- reactive({
    
    #if(is.null(input$df)){ return()}
    req(input$df)
    
    # Se o dado for em nivel de arvore, a totalização de parcelas deve ser feita para que
    # NewData possa ser inserido em acs. Sem essa condição a ui gera mensagens de erro
    switch(input$df, 
           "Dados em nivel de fuste" = if(is.null(tot_arvoreData()) ){return()}else{ tot_arvoreData()},
           "Dados em nivel de arvore" = rawData() )
    
    
  })

  # Índices de diversidade ####
  
  # funcao diversidade
  tabdiversidade <- reactive({
    
    nm <- varnames()
    dados <- arvData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$especies,"Por favor mapeie a coluna referente a 'especies'  ") )
    
    # Se o usuario nao quiser realizar a anlise por parcela, o elemento parcelas da lista sera nulo,
    # mesmo que o usuario tenha mapeado a variavel parcela na aba de mapeamento.
     if(input$rb_div=="Nao"){
       grupo=NULL
       }else if(input$rb_div=="Parcela"){
         grupo=nm$parcelas
       }else if(input$rb_div=="Estrato"){
         grupo=nm$estrato
       }
    
      x <- diversidade(data             = dados, 
                       col.especies     = nm$especies, 
                       col.parcelas     = grupo, 
                       rotulo.NI        = nm$NI  ) # %>% 
      #gather("Índice", "Resultado") # transpor tabela
      x 
    
  })
  
  # tabela diversidade
  output$div <- DT::renderDataTable({
    
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
  
  # Índices de similaridade ####
  
  tabmsimilaridade <- reactive({
    
    nm <- varnames()
    dados <- arvData()
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$especies,"Por favor mapeie a coluna referente a 'especies'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  ") )
    
    if(input$rb_sim=="Parcela"){
      grupo=nm$parcelas
    }else if(input$rb_sim=="Estrato"){
      grupo=nm$estrato
    }
    
    
    x <- m.similaridade(data             = dados, 
                        col.especies     = nm$especies,
                        col.comparison   = grupo,
                        rotulo.NI        = nm$NI  )
    
    x 
    
    
  })

  # Tabelas
  output$msim1 <- DT::renderDataTable({
    
    
      x <- tabmsimilaridade()
      x <- as.data.frame(x[[1]])
      names(x) <- 1:length(x)
      
      
      msimdt1 <- tibble::rownames_to_column(x, " ") 
      
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
      
  }) 
  output$msim2 <- DT::renderDataTable({
    
      x <- tabmsimilaridade()
      x <- as.data.frame(x[[2]])
      names(x) <- 1:length(x)
      
      msimdt2 <- tibble::rownames_to_column(x, " ") 
      
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

  }) 
  
  # esses renderUI nao sao utilizadas, estao sendo mantidas apenas para caso sejam utilizadas
  output$rb_graphmsim <- renderUI({
    
    # precisa que o grafico seja selecionado na ui, caso contrario nao mostra nada
    req(input$mainPanel_Indices %in% c("id_msim1_graph", "id_msim2_graph") ||  input$graph_d %in% c("Dendrograma - Jaccard","Dendrograma - Sorensen") )
      radioButtons("rb_msim_graph", 
                   "Selecione o método de classificação:", 
                   c("Vizinho mais próximo"  = "single", 
                     "Vizinho mais distante" = "complete", 
                     "Distância euclidiana"  = "average"), 
                   selected = "complete", inline = T) 
      })
  output$slider_graphmsim <- renderUI({
    
    # precisa que o grafico seja selecionado na ui, caso contrario nao mostra nada
    req(input$mainPanel_Indices %in% c("id_msim1_graph", "id_msim2_graph") ||  input$graph_d %in% c("Dendrograma - Jaccard","Dendrograma - Sorensen") )
    
       sliderInput("slider_msim_graph", 
                  label = "Selecione o número de clusters:", 
                  min = 1, 
                  max = 10, 
                  value = 3,
                  step = 1) 
    
  })
  
  # Graficos
  msim1_graph <- reactive({
    
    #retornar vazio enquando input$rb_msim1_graph carrega (ele fica nulo quando carrega)
    #if(is.null(input$rb_msim1_graph)){return("")} 
    
    req(input$rb_msim_graph,input$slider_msim_graph )
    
    nm <- varnames()
    dados <- arvData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$especies,"Por favor mapeie a coluna referente a 'especies'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  ") )
    
      df <- as.data.frame(tabmsimilaridade()[[1]] ) 
      
      rownames(df) <- levels( as.factor( dados[,nm$parcelas] ) )
      
      hc    <- hclust(dist(df), input$rb_msim_graph) # heirarchal clustering
      dendr <- ggdendro::dendro_data(hc) # convert for ggplot
      clust    <- cutree(hc,k=input$slider_msim_graph)                    # find 2 clusters
      clust.df <- data.frame(label=names(clust), cluster=factor(clust))
      
      # dendr[["labels"]] has the labels, merge with clust.df based on label column
      dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
      # plot the dendrogram; note use of color=cluster in geom_text(...)
      
      x <- ggdendro::ggdendrogram(dendr) +
        geom_text(data=ggdendro::label(dendr), aes(x, y, label=label, hjust=.5,color=cluster), size=4) +
        ggdendro::theme_dendro()
      
      x
      
  })
  output$msim1_graph_ <- renderPlot({
    
    gmsim1 <- msim1_graph()
    
    gmsim1
    
  })
  
  msim2_graph <- reactive({
    
    #retornar vazio enquando input$rb_msim1_graph carrega (ele fica nulo quando carrega)
    #if(is.null(input$rb_msim2_graph)){return("")} 
    
    req(input$rb_msim_graph,input$slider_msim_graph )
    
    nm <- varnames()
    dados <- arvData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$especies,"Por favor mapeie a coluna referente a 'especies'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  ") )
    
    df <- as.data.frame(tabmsimilaridade()[[2]] ) 
    
    rownames(df) <- levels( as.factor( dados[,nm$parcelas] ) )
    
      hc    <- hclust(dist(df), input$rb_msim_graph) # heirarchal clustering
      dendr <- ggdendro::dendro_data(hc) # convert for ggplot
      clust    <- cutree(hc,k=input$slider_msim_graph) 
      clust.df <- data.frame(label=names(clust), cluster=factor(clust))
      
      # dendr[["labels"]] has the labels, merge with clust.df based on label column
      dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
      # plot the dendrogram; note use of color=cluster in geom_text(...)
      
      x <- ggdendro::ggdendrogram(dendr) +
        geom_text(data=ggdendro::label(dendr), aes(x, y, label=label, hjust=.5,color=cluster), size=4) +
        ggdendro::theme_dendro()
      
      x   
      

  })
  output$msim2_graph_ <- renderPlot({
    
    gmsim2 <- msim2_graph()
    
    gmsim2
    
    
  })
  
  # Índices de agregação ####
  
  # funcao agregate
  tabagregate <- reactive({
    
    nm <- varnames()
    dados <- arvData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$especies,"Por favor mapeie a coluna referente a 'especies'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcela'  ") )
    
    x <- agregacao(data         = dados, 
                   col.especies = nm$especies, 
                   col.parcelas = nm$parcelas, 
                   rotulo.NI    = nm$NI  )
    
    x
    
  })
  output$agreg <- renderDataTable({
    
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
  
  # Analise estrutural ####
  
  # funcao estrutura
  tabestrutura <- reactive({
    
    dados <- arvData()
    nm <- varnames()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$especies,"Por favor selecione a coluna referente a 'especies'  "),
      need(nm$parcelas,"Por favor selecione a coluna referente a 'parcelas'  "),
      need(nm$dap,"Por favor selecione a coluna referente a 'dap'  "),
      need(nm$area.parcela,"Por favor selecione a coluna referente a 'area da parcela'  ")
      
      )
    
    x <- estrutura(data             = dados, 
                   col.especies     = nm$especies,
                   col.parcelas     = nm$parcelas,
                   col.dap          = nm$dap,
                   area.parcela     = nm$area.parcela,
                   est.vertical     = nm$est.vertical,
                   est.interno      = nm$est.interna,
                   nao.identificada = nm$NI  )
    
    as.tbl(x)
    
  })
  
  # tabela estrutura
  output$estr <- renderDataTable({
    
    estrdt <- round_df( tabestrutura(), 4 )
   # estrdt <- tabestrutura()
    
    datatable( as.tbl(estrdt),
               options = list(searching = T,
                              paging=T,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )  ) 
    
  }) 

  # grafico estrutura (IVI)
  output$ivi_graph_opts <- renderUI({
    
    #req(input$mainPanel_Estrutural=="Gráfico IVI")
    
    dados <- rawData()
    nm <- varnames()
    
    n_max <- nlevels(as.factor(dados[[nm$especies]]))
    
    
    list(
    column(width=3,
           h3("Configuração do gráfico IVI:")
    ),
    column(3,
           numericInput("n_IVI_g", h4("Número de espécies no eixo y:"),10,1,n_max,1) ),
    
    column(3,
           radioButtons("g_ivi_bw", 
                        "Gráfico em tons de cinza?", 
                        c("Sim"=T,"Nao"=F),
                        selected = F,
                        inline = T))
    
    )
    
    
  })
  ivi_graph <- reactive({

    validate(
      need(input$n_IVI_g, ""),
     # need(input$g_ivi_bw, ""),
      need(tabestrutura(), "Por favor faça a análise estrutural")  )
    
     g <- tabestrutura() %>% 
      arrange(-IVI) %>% 
      mutate(n = as.numeric(row.names(.)), 
             class = ifelse(n>input$n_IVI_g,"Demais especies",as.character(especie)),
             class = factor(class, levels=unique(class)) ) %>% 
      gather(IVI_contrib, valor, FR , DR , DoR, factor_key = T) %>% 
      group_by(class, IVI_contrib) %>% 
      summarise(valor_d = sum(valor), IVI = sum(IVI), IVI_sep = valor_d/3,IVI_contrib_porc = round(valor_d/3/IVI,2)) %>% 
      ggplot(aes( ordered(class, levels = rev(levels(class)) ) , IVI_sep, fill=IVI_contrib ) ) + 
      geom_bar(stat = "identity", width = .8, color = "black") +
      # geom_text(aes(label = scales::percent(IVI_contrib_porc) ), position = position_stack(vjust = 0.5), size = 4) + 
      coord_flip() +
      labs(x = "Espécies", y="IVI", fill = "Legenda") +
      ggthemes::theme_igray(base_family = "serif") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size=22, face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   ) + 
      guides(fill = guide_legend(reverse=T))
    
     if(input$g_ivi_bw){g <- g + ggplot2::scale_fill_grey(start = 0.8, end = 0.2) }
     
     g
  })
  output$estrg <- renderPlot({
    
    ivi_graph()
    
  })

  # grafico estrutura vertical
  
  est.vert_graph <- reactive({
    
    dados <- arvData()
    nm <- varnames()
    
    validate(
      need(dados, "Por favor faça a análise estrutural"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(nm$est.vertical, "Por favor defina a estrutura vertical")  )
      
    dados %>% 
      rename(xvar = !!rlang::sym(nm$est.vertical)  ) %>% 
    ggplot(aes(x=as.factor(xvar) ) ) + 
      geom_bar(stat="count", color = "black") +
      labs(x = "Estrutura vertical", y="Número de indivíduos") +
      ggthemes::theme_igray(base_family = "serif") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size=22, face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   ) + 
      guides(fill = guide_legend(reverse=T))
    
    
    
  })
  output$est.vert_plot <- renderPlot({
    
    est.vert_graph()
    
  })
  
  # Distribuicao diametrica ####
  
  dd_list <- reactive({
    
    nm <- varnames()
    dados <- rawData()

    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  ") )
    
    lista <- list()
    lista[["dd_geral"]] <- classe_diametro(df = dados, 
                                           dap = nm$dap,
                                           parcela = nm$parcelas,
                                           area_parcela = nm$area.parcela, 
                                           ic = nm$IC, 
                                           dapmin = nm$diam.min, 
                                           especies = NA, 
                                           volume = nm$vcc,
                                           rotulo.NI = nm$NI,
                                           keep_unused_classes = TRUE
                                           )
   
    lista[["dd_especie"]] <- classe_diametro(df = dados, 
                                           dap = nm$dap,
                                           parcela = nm$parcelas,
                                           area_parcela = nm$area.parcela, 
                                           ic = nm$IC, 
                                           dapmin = nm$diam.min, 
                                           especies = nm$especies, 
                                           volume = nm$vcc,
                                           rotulo.NI = nm$NI,
                                           keep_unused_classes = TRUE
    )
    
    lista[["dd_especie_indv_cc_column"]] <- classe_diametro(df = dados, 
                                             dap = nm$dap,
                                             parcela = nm$parcelas,
                                             area_parcela = nm$area.parcela, 
                                             ic = nm$IC, 
                                             dapmin = nm$diam.min, 
                                             especies = nm$especies, 
                                            # volume = NA,
                                             rotulo.NI = nm$NI,
                                             cc_to_column = T,
                                             cctc_ha = T,
                                             keep_unused_classes = TRUE
                                                                     )
    
    lista[["dd_especie_vol_cc_column"]] <- classe_diametro(df = dados, 
                                                      dap = nm$dap,
                                                      parcela = nm$parcelas,
                                                      area_parcela = nm$area.parcela, 
                                                      ic = nm$IC, 
                                                      dapmin = nm$diam.min, 
                                                      especies = nm$especies, 
                                                      volume = nm$vcc,
                                                      rotulo.NI = nm$NI,
                                                      cc_to_column = T,
                                                      cctc_ha = T,
                                                      keep_unused_classes = TRUE
                                                                   )
    
    lista[["dd_especie_G_cc_column"]] <- classe_diametro(df = dados, 
                                                           dap = nm$dap,
                                                           parcela = nm$parcelas,
                                                           area_parcela = nm$area.parcela, 
                                                           ic = nm$IC, 
                                                           dapmin = nm$diam.min, 
                                                           especies = nm$especies, 
                                                          # volume = NA,
                                                           rotulo.NI = nm$NI,
                                                           cc_to_column = T,
                                                           G_to_cc = T,
                                                           cctc_ha = T,
                                                         keep_unused_classes = TRUE
                                                      )
    
    lista
  })
  
  output$dd_geral_tab <- DT::renderDataTable({
    
    g <- round_df(dd_list()[["dd_geral"]], 2)
    
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
             )
               
    
  })
  output$dd_indv_especie_tab <- DT::renderDataTable({
    
    g <- round_df(dd_list()[["dd_especie_indv_cc_column"]], 2)
    
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  output$dd_vol_especie_tab <- DT::renderDataTable({
    nm <- varnames()
    validate(need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "))
    
    g <- round_df(dd_list()[["dd_especie_vol_cc_column"]], 2)
    
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  output$dd_G_especie_tab <- DT::renderDataTable({
    
    g <- round_df(dd_list()[["dd_especie_G_cc_column"]], 4)
    
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  
  dd_g1 <- reactive({
    
    g <- dd_list()[["dd_geral"]]
    #g$CC2 <-  sapply(g$CC , gsub, pattern= "[.]",replacement= "," )
    
    ggplot(g, aes(as.factor(CC),IndvHA)) +
      geom_bar(stat = "identity",color="black")+
   #   scale_y_continuous( expand=c(0,15) ) +
      ggthemes::theme_igray(base_family = "serif") +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Nº de Individuos por hectare") + 
      geom_text(aes(label = round(IndvHA,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
    
  })
  output$dd_graph_indv <- renderPlot({
    
    dd_g1()
    
    
  })
  dd_g2 <- reactive({
    
    nm <- varnames()
    validate(need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "))
    
    g <- dd_list()[["dd_geral"]]
    
    ggplot(g, aes(as.factor(CC),volume_ha)) +
      geom_bar(stat = "identity",color="black")+
    #  scale_y_continuous( expand=c(0,15) ) +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Volume por hectare") + 
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(volume_ha,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
  })
  output$dd_graph_vol <- renderPlot({
    
    dd_g2()
    
  })
  dd_g3 <- reactive({
   
    g <- dd_list()[["dd_geral"]] 
    
    ggplot(g, aes(as.factor(CC),G_ha)) +
      geom_bar(stat = "identity",color="black")+
     # scale_y_continuous( expand=c(0,15) ) +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Área Basal (G) por hectare") + 
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(G_ha,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
  })
  output$dd_graph_G <- renderPlot({
    
    dd_g3()
    
  })
  
  # BDq ####
  
  # Tabelas BDq
  BDq_list <- reactive({
    nm <- varnames()
    dados <- rawData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  ") )

    x <- bdq.meyer(data             = dados, 
                   col.parcelas     = nm$parcelas,
                   col.dap          = nm$dap,
                   area.parcela     = nm$area.parcela,
                   intervalo.classe = nm$IC,
                   min.dap          = nm$diam.min,
                   i.licourt        = input$i.licourtBDq  )
    
    x
    #x[[1]]
    
  })
  output$BDq1 <- renderDataTable({
    
    BDqdt <- BDq_list()[[1]]
    
    datatable( as.data.frame(BDqdt),
               options = list(searching = T,
                              rownames = F,
                              paging=T,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )  ) 
    
  }) 
  output$BDq3 <- renderDataTable({
    
    BDqdt <- BDq_list()[[3]]
    
    
    BDqdt <-data.frame( "Coeficientes" = c("b0", "b1")  ,
                        "Valor"        = c( BDqdt[1], BDqdt[2] )  )
    
    
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
    
    req( BDq_list() )
    
    data <- BDq_list()[[1]]
    
    graph_bdq <- data %>% 
      select("classe_de_diametro"      = CentroClasse, 
             "Distribuição observada"  = IndvHectare , 
             "Distribuição balanceada" = MeyerBalan  ) %>% 
      gather(class, num_indv_ha, -classe_de_diametro, factor_key = T) %>% 
      arrange(classe_de_diametro) %>% 
      mutate(classe_de_diametro = as.factor(classe_de_diametro) )
    
    g <-  ggplot(graph_bdq, aes(x = classe_de_diametro, y = num_indv_ha) ) + 
      geom_bar(aes(fill = class), stat = "identity",position = "dodge", color='black') +
      labs(x = "Classe de diâmetro (cm)", y = "Número de indivíduos (ha)", fill = NULL) + 
      scale_fill_manual(values =c("#108e00", "cyan3","firebrick2") ) +
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(num_indv_ha,1), group=class ), position = position_dodge(width = 1), vjust = -0.3, size = 6 ) + 
      theme(
        legend.position="bottom",
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )

    g
    
  })
  
  output$BDq_graph_ <- renderPlot({
    #plotly::renderPlotly
    req(BDq_graph())
    
    g <- BDq_graph()
    
    #  plotly::ggplotly(p=g) 
    g
    
  })
  
  # totalizacao de parcelas ####
  
  tot_parcData <- reactive({  
    
    nm <- varnames()
    dados <- rawData()

    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(nrow(dados)>0, "Base de dados vazia"),
      need(input$df != "Dados em nivel de parcela", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$parcelas,"Por favor mapeie a coluna referente a 'parcelas'  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  ")
    )
    
    # Verificar se caso o usuario escolha volume como variavel para o inventario
    # esta deve ser mapaeada anteriormente
    validate(check_yi(nm$vcc, input$yi_inv), errorClass = "WRONG")
    
    # Se o usuario inseir uma variavel de Estrato, considera-la na hora dos calculos
    if( any(nm$estrato =="") ){grupos<-nm$parcela}else{grupos <- c(nm$estrato, nm$parcela)}
    
    x <- inv_summary(df           = dados, 
                     DAP          = nm$dap, 
                     HT           = nm$ht,
                     VCC          = nm$vcc,
                     area_parcela = nm$area.parcela,
                     .groups      = grupos,
                     area_total   = nm$area.total,
                     idade        = NA,
                     VSC          = NA,
                     Hd           = NA) %>% 
      dplyr::ungroup()
    
    x
    
  }) 
  output$tot_parc_tab <- renderDataTable({ # renderizamos uma DT::DataTable
    
    data <- tot_parcData() 
    
    datatable(data,
              options = list(initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                "}"),
                pageLength = 25
              )   
    ) # Criamos uma DT::datatable com base no objeto
    
  })
  
  # Switch para trocar o dado utilizado no inventario ####
  
  invData <- reactive({
    
    #if(is.null(input$df)){ return()}
    req(input$df)
    
    # Se o dado for em nivel de arvore, a totalização de parcelas deve ser feita para que
    # NewData possa ser inserido em acs. Sem essa condição a ui gera mensagens de erro
    switch(input$df, 
           "Dados em nivel de fuste" = if(is.null(tot_parcData()) ){return()}else{ tot_parcData()},
           "Dados em nivel de arvore" = if(is.null(tot_parcData()) ){return()}else{ tot_parcData()},
           "Dados em nivel de parcela" = rawData() )
    
  })
  
  # amostragem casual simples ####
 
  # UI para rodar acs por estrato
  output$acs_estrato_rb <- renderUI({
    
    req(input$tabset_inv=="Amostragem casual simples")
    
    radioButtons("acs_estrato", 
                 "Calcular uma amostragem casual simples para cada estrato?",
                 choices = c("Sim"=T,"Nao"=F),
                 selected = F,
                 inline = T)
    
  })
  output$acs_as_warning <- renderUI({
    
    req(any( c(input$acs_estrato==T, input$as_estrato==T)   ), # Precisa que o usuario tente calcular acs ou as por estrato
        input$tabset_inv %in% c("Amostragem casual simples", "Amostragem sistemática") ) # precisa que a aba acs ou as seja selecionada
    validate(
      need(!is.null(input$col.estrato) , # estrato nao e nulo? quando a resposta for nao a mensagem aparece
           "Variável 'estrato' não definida. A amostragem será feita para todos os dados." ), errorClass = "AVISO")
    
    
    
  })
  
  # funcao acs aplicada em invData
  tabacs <- reactive({
    
    nm <- varnames()
    dados <- invData()
    
    validate(
      need(dados, "Por favor, faça a totalização de parcelas, ou o upload de uma base de dados em nível de parcela" ),
      need(nrow(dados)>0, "Base de dados vazia"),
      #need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  ")
    )
    
    grupos_name <- NULL
    # Fazer amostragem por estrato somente se o usuario marcar sim
    if(is.null(input$acs_estrato)){
      
    }else if(input$acs_estrato){
      grupos_name <- nm$estrato
    }
    
    if(input$df=="Dados em nivel de parcela"){
      dados <- dados %>% dplyr::rename(VCC = !!(rlang::sym(nm$vcc)) )
    }
    
    x <-     acs(df             = dados,
                 Yi             = input$yi_inv,
                 area_parcela   = nm$area.parcela,
                 area_total     = nm$area.total, 
                 #      idade          = nm$idade,
                 .groups        = grupos_name, 
                 alpha          = input$alpha_inv, 
                 erro           = input$erro_inv, 
                 casas_decimais = input$cd_inv, 
                 pop            = input$pop_inv, 
                 tidy           = TRUE)
    
    x
    
  })
  # tabela acs
  output$acs <- renderDataTable({
    
    acsdt <- tabacs() 
    # converte em datatable        # cria formattable
    as.datatable( formattable(acsdt, 
                              list(    # colore a linha 6 da coluna dois de verde ou vemelho, se ela for menor ou maior que o numero da linha 1 coluna 2
                                area(row=6, col=2) ~  formatter("span", 
                                                                style = x ~ formattable::style(color = ifelse(x <= acsdt[1,2], "#108e00", "red"))) ,
                                # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                area(row=10, col=2) ~ formatter("span", 
                                                                style = x ~ formattable::style(color = ifelse(x <= input$erro_inv, "#108e00", "red")))
                                
                                
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
  
  # Amostragem casual estratificada ####
  
  # funcao ace aplicada em invData
  list_ace <- reactive({
    
    nm <- varnames()
    dados <- invData()
    
    validate(
      need(dados, "Por favor, faça a totalização de parcelas, ou o upload de uma base de dados em nível de parcela" ),
      need(nrow(dados)>0, "Base de dados vazia"),
      #need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  "),
      need(nm$estrato,"Por favor mapeie a coluna referente a 'Estrato' ")
    )
    
    if(input$df=="Dados em nivel de parcela"){
      dados <- dados %>% dplyr::rename(VCC = !!(rlang::sym(nm$vcc)) )
    }
    
    x <- ace(df             = dados, 
             Yi             = input$yi_inv,
             area_parcela   = nm$area.parcela, 
             area_estrato   = nm$area.total, 
             .groups        = nm$estrato, 
             # idade          = nm$idade, 
             alpha          = input$alpha_inv, 
             erro           = input$erro_inv, 
             casas_decimais = input$cd_inv, 
             pop            = input$pop_inv, 
             tidy           = TRUE)
    x
    
  })
  # tabela ace1
  output$ace1 <- renderDataTable({
    
    ace1dt <- list_ace()[[1]] 
    
    datatable( ace1dt, # seleciona a linha 5 previamente
               selection = list(mode = 'multiple', selected = c(14,18,19,20), target = 'row'),
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
    
    ace2dt <- list_ace()[[2]] 
    
    # converte em datatable        # cria formattable
    as.datatable( formattable(ace2dt, 
                              list(
                                # colore o erro estimado de verde ou vemelho, se ela for menor ou maior que o erro desejado
                                area(row=5, col=2) ~ formatter("span", 
                                                               style = x ~ formattable::style(color = ifelse(x <= input$erro_inv, "#108e00", "red")))
                                
                                
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
  
  # Amostragem sistematica ####
  
  # UI para rodar as por estrato
  output$as_estrato_rb <- renderUI({
    
    req(input$tabset_inv=="Amostragem sistemática")
    
    radioButtons("as_estrato", 
                 "Calcular uma amostragem sistematica para cada estrato?",
                 choices = c("Sim"=T,"Nao"=F),
                 selected = F,
                 inline = T)
    
  })
  
  # funcao as aplicada em invData
  tabas <- reactive({
    
    nm <- varnames()
    dados <- invData()
    
    validate(
      need(dados, "Por favor, faça a totalização de parcelas, ou o upload de uma base de dados em nível de parcela" ),
      need(nrow(dados)>0, "Base de dados vazia"),
      #need(nm$vcc,"Por favor mapeie a coluna referente a 'volume com casca' ou estime-o na aba preparação  "),
      need(nm$area.parcela,"Por favor mapeie a coluna ou insira um valor referente a 'area.parcela'  "),
      need(nm$area.total,"Por favor mapeie a coluna ou insira um valor referente a 'area.total'  ")
    )
    
    grupos_name <- NULL
    # Fazer amostragem por estrato somente se o usuario marcar sim
    if(is.null(input$as_estrato)){
      
    }else if(input$as_estrato){
      grupos_name <- nm$estrato
    }
    
    dados <- invData()
    
    if(input$df=="Dados em nivel de parcela"){
      dados <- dados %>% dplyr::rename(VCC = !!(rlang::sym(nm$vcc)) )
    }
    
    x <- as_diffs(df             = dados, 
                  Yi             = input$yi_inv,
                  area_parcela   = nm$area.parcela,
                  area_total     = nm$area.total, 
                  # idade          = nm$idade,
                  .groups        = grupos_name, 
                  alpha          = input$alpha_inv, 
                  erro           = input$erro_inv, 
                  casas_decimais = input$cd_inv, 
                  tidy           = TRUE )
    
    x
    
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
                                                                style = x ~ formattable::style(color = ifelse(x <= input$erro_inv, "#108e00", "red")))
                                
                                
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
  
  # Cria um valor inicial zero para verificar se o usuario fez algum download ou nao.
  # Se o usuario clicar em algum botao de download, sera add a esse valor uma unidade.
  rnDownloads <- reactiveValues(ndown=0)
  
  output$checkbox_df_download <- renderUI({
    
    checkboxGroupInput("dataset", h3("Escolha uma ou mais tabelas, e clique no botão abaixo:"), 
                       choices =  c(
                         "Dados inconsistentes"              ,
                         "Dado nivel arvore"                    ,
                         "Indice diversidade"                ,
                         "Matriz similaridade - Jaccard"     ,
                         "Matriz similaridade - Sorensen"    ,
                         "Indice de agregacao"               ,
                         "Estrutura"                         ,
                         "Distribuicao diametrica geral"     ,
                         "Dist. Diametrica Indv. por especie",
                         "Dist. Diametrica Vol. por especie" ,
                         "Dist. Diametrica G por especie"    ,
                         "BDq Meyer"                         ,
                         "BDq Meyer - Coeficientes"          ,
                         "Totalizacao de parcelas"           ,
                         "Amostragem Casual Simples"         ,
                         "Amostragem Casual Estrat 1" ,
                         "Amostragem Casual Estrat 2" ,
                         "Amostragem Sistematica"            
                       ), inline = T )
    
    
  })
  
  list_of_df_to_download <- reactive({
    
    L <- list()
    
    if("Dados inconsistentes" %in% input$dataset ) {
      L[["Dados inconsistentes"]] <- try( consist_fun(), silent = T) 
    }
    
    if("Dado nivel arvore" %in% input$dataset ) {
      L[["Dado nivel arvore"]] <-  try(arvData(), silent = T)
    }
   
    if("Indice diversidade" %in% input$dataset ) {
      L[["Indice diversidade"]] <-  try(tabdiversidade(), silent=T)
    }
    
    if("Matriz similaridade - Jaccard" %in% input$dataset ) {
      L[["Matriz similaridade - Jaccard"]] <-  try(tibble::rownames_to_column(as.data.frame(tabmsimilaridade()[[1]]), " "), silent=T)
    }
    
    
    if("Matriz similaridade - Sorensen" %in% input$dataset ) {
      L[["Matriz similaridade - Sorensen"]] <- try(tibble::rownames_to_column(as.data.frame(tabmsimilaridade()[[2]]), " ") , silent=T)
    }
    
    if("Indice de agregacao" %in% input$dataset ) {
      L[["Indice de agregacao"]] <-  try(tabagregate(), silent=T)
    }
    
    if("Estrutura" %in% input$dataset ) {
      L[["Estrutura"]] <- try(tabestrutura() , silent = T)
    }
    
    if("Distribuicao diametrica geral" %in% input$dataset ) {
      L[["Distribuicao diametrica geral"]] <-  try(dd_list()[["dd_geral"]], silent=T)
    }
    
    if("Dist. Diametrica Indv. por especie" %in% input$dataset ) {
      L[["Dist. Diametrica Indv. por especie"]] <- try(dd_list()[["dd_especie_indv_cc_column"]] , silent=T)
    }
    
    if("Dist. Diametrica Vol. por especie" %in% input$dataset ) {
      L[["Dist. Diametrica Vol. por especie"]] <- try(dd_list()[["dd_especie_vol_cc_column"]]  , silent=T)
    }
    
    if("Dist. Diametrica G por especie" %in% input$dataset ) {
      L[["Dist. Diametrica G por especie"]] <- try(dd_list()[["dd_especie_G_cc_column"]], silent=T) 
    }
    
    if("BDq Meyer" %in% input$dataset ) {
      L[["BDq Meyer"]] <-   try(BDq_list()[[1]], silent=T)
    }
    
    if("BDq Meyer - Coeficientes"  %in% input$dataset ) {
      L[["BDq Meyer - Coeficientes" ]] <- try( data.frame( "Coeficientes" = c("b0", "b1"),"Valor"= c( BDq_list()[[3]][1], BDq_list()[[3]][2] )), silent=T)
    }
    
    if("Totalizacao de parcelas" %in% input$dataset ) {
      L[["Totalizacao de parcelas"]] <- try(tot_parcData() , silent=T) 
    }
    
    if("Amostragem Casual Simples" %in% input$dataset ) {
      L[["Amostragem Casual Simples"]] <- try(tabacs() , silent=T)
    }
    
    if("Amostragem Casual Estrat 1" %in% input$dataset ) {
      L[["Amostragem Casual Estrat 1"]] <- try(list_ace()[[1]], silent = T)
    }
    
    if("Amostragem Casual Estrat 2" %in% input$dataset ) {
      L[["Amostragem Casual Estrat 2"]] <- try(list_ace()[[2]] , silent=T)
    }
    
    if("Amostragem Sistematica" %in% input$dataset ) {
      L[["Amostragem Sistematica"]] <- try( tabas() , silent=T)
    }
    
    # Remover dataframes que geraram errol
    L <- L[!sapply(L, is,"try-error")]
    
    L
     
  })
  list_of_df_all <- reactive({
    
    L <- list()
    
      L[["Dados inconsistentes"]] <- try( consist_fun(), silent = T) 

    
      L[["Dado nivel arvore"]] <-  try(arvData(), silent = T)

      L[["Indice diversidade"]] <-  try(tabdiversidade(), silent=T)

      L[["Matriz similaridade - Jaccard"]] <-  try(tibble::rownames_to_column(as.data.frame(tabmsimilaridade()[[1]]), " "), silent=T)

      L[["Matriz similaridade - Sorensen"]] <- try(tibble::rownames_to_column(as.data.frame(tabmsimilaridade()[[2]]), " ") , silent=T)

      L[["Indice de agregacao"]] <-  try(tabagregate(), silent=T)

      L[["Estrutura"]] <- try(tabestrutura() , silent = T)

      L[["Distribuicao diametrica geral"]] <-  try(dd_list()[["dd_geral"]], silent=T)

      L[["Dist. Diametrica Indv. por especie"]] <- try(dd_list()[["dd_especie_indv_cc_column"]] , silent=T)

      L[["Dist. Diametrica Vol. por especie"]] <- try(dd_list()[["dd_especie_vol_cc_column"]]  , silent=T)

      L[["Dist. Diametrica G por especie"]] <- try(dd_list()[["dd_especie_G_cc_column"]], silent=T) 

      L[["BDq Meyer"]] <-   try(BDq_list()[[1]], silent=T)

      L[["BDq Meyer - Coeficientes" ]] <- try( data.frame( "Coeficientes" = c("b0", "b1"),"Valor"= c( BDq_list()[[3]][1], BDq_list()[[3]][2] )), silent=T)

      L[["Totalizacao de parcelas"]] <- try(tot_parcData() , silent=T) 

      L[["Amostragem Casual Simples"]] <- try(tabacs() , silent=T)

      L[["Amostragem Casual Estrat 1"]] <- try(list_ace()[[1]], silent = T)

      L[["Amostragem Casual Estrat 2"]] <- try(list_ace()[[2]] , silent=T) 

      L[["Amostragem Sistematica"]] <- try( tabas() , silent=T)

      # Remover dataframes que geraram errol
      L <- L[!sapply(L, is,"try-error")]
      
      L

  })
  output$downloadData <- downloadHandler(
    filename = function(){"tabelas_app_nativas.xlsx"},
    
    content = function(file){
      rnDownloads$ndown <- rnDownloads$ndown + 1
      suppressWarnings(openxlsx::write.xlsx( list_of_df_to_download(), file ))}
    
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function(){"tabelas_app_nativas.xlsx"},
    
    content = function(file){ 
      rnDownloads$ndown <- rnDownloads$ndown + 1
      suppressWarnings(openxlsx::write.xlsx( list_of_df_all(), file )) }
    
  )
  
  # Download graficos ####
  
  graphInput <- reactive({
    switch(input$graph_d,
           "Dendrograma - Jaccard"      = msim1_graph(),
           "Dendrograma - Sorensen"     = msim2_graph(),
           "Grafico IVI"                = ivi_graph(),
           "Grafico Estrutura Vertical" = est.vert_graph(),
           "Indv. por ha por CC"        = dd_g1(),
           "Vol. por ha por CC"         = dd_g2(),
           "G por ha por CC"            = dd_g3(),
           "Distribuicao - BDq Meyer"   = BDq_graph() )
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
      rnDownloads$ndown <- rnDownloads$ndown + 1
      
      ggsave(file, graphInput(), width = 12, height = 6 )
      
      
    }
  )
  # session end ####
 # session$onSessionEnded(function() {
  #  stopApp()
  #  q("no")
  #})
  # ####
})

