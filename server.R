library(shiny)
library(DT)
#library(plotly)
library(formattable)
library(readxl)
#library(plyr)
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
source("funs/estrat_vert_souza.R"  , encoding="UTF-8")

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

  
  # Importação ####
  
  #ui
  output$upload      <- renderUI({
    
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
  
  #tabela
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
    # sera vazio caso nao seja selecionado "fazer o upload"
    validate(need(input$df_select == "Fazer o upload" , "" )  )
    
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
  
  # render table
  output$rawdata <- renderDataTable({ # renderizamos uma DT::DataTable
    
    validate(need(!is.null(rawData_()), "Please import a dataset"))
    
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
  
  # Mapeamento ####
  
  # ui
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
  output$selec_dap          <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dap", # Id
      NULL, # nome que sera mostrado na UI
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
  
  output$selec_area.parcela <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.area.parcela",
                   NULL, # nome que sera mostrado na UI
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
                   NULL, # nome que sera mostrado na UI
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
  output$selec_agrup        <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.agrup",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                   selected = estratos_names,
                   multiple = T,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  
  output$selec_est.vertical <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.est.vertical",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                  # selected =  ,
                   multiple = T,
                   options = list(
                     maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
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

  # Preparação ####
  # ui
  output$selec_rotuloNI     <- renderUI({
    
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
    
    selectizeInput("level.rm_data_level",
                   label = "Selecione o(s) nivel(s) que se deseja remover:",
                   choices = opcoes,
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione o(s) nivel(s) abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
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
      
      h3("Área da parcela (numérico)"),
      
      
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
      h3("Área total (numérico)"),
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'num.area.total', # Id
        "Insira o valor para a Área total:", # nome que sera mostrado na UI
        value = "", 
        step = 1
      )
      
    )
    
  })
     # Calculo de volume 
    output$ui_estvol1 <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    req(is.null(input$col.vcc) || input$col.vcc =="" )

    data <- rawData_()
    
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
                   ) )      )
      

    
  })
  output$ui_estvol3 <- renderUI({
    
    # precisa que o usuario nao tenha selecionado o volume
    req(is.null(input$col.vcc) || input$col.vcc =="" )
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b0_estvol', # Id
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
    
    # precisa que o usuario nao tenha selecionado o volume
    req(is.null(input$col.vcc) || input$col.vcc =="" )
    # Precisa ter b2 no modelo
    req( grepl( "\\<b2\\>",input$modelo_estvol) ) 
    
    list(
      
      numericInput( # cria uma lista de opcoes em que o usuario pode clicar
        'b2_estvol', # Id
        "Insira o valor para o b2:", # nome que sera mostrado na UI
        value = "", 
        step = 0.0001
      )
      
    )
    
  })

    # calcular estrutura vertical
  output$checkbox_calc.est.vert <- renderUI({
    
    # precisa que o usuario nao tenha selecionado estrutura vertical E tenha selecionado altura
    req((is.null(input$col.est.vertical) || input$col.est.vertical=="") &&  (!is.null(input$col.ht) || input$col.ht!="")   )
    
    list(
    
    h3("Calcular Estrutura interna"),
    
    h5("A estrutura interna será calculada utilizando a variável altura, segundo o método de Souza (2002)"),
      
    radioButtons("est.vert.calc",
                  "Deseja classificar a estrutura interna utilizando a variável altura?",
                 c("Sim", "Nao"), "Nao" )
    )
    
  })
  
  # tabela
  # rawData sera o dado utilizado durante o resto do app
  # as alteracoes feitas em 'preparacao' serao salvas aqui
  # caso nao seja feito nada, rawData sera identico a rawData_
  rawData <- reactive({
    
    data <- rawData_()
    
    # o primeiro if sera para remover as linhas
    
    # se o usuario nao selecionar nada, retorna o dado normal 
    # (isso faz com o que o dado original seja exibido logo que se entra na aba de filtrar),
    # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis
    
    if( is.null(input$col.rm_data_var) || input$col.rm_data_var ==""){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
      
    }else{
      
      # remove linhas caso um nivel seja selecionado
      data <- data[!data[[input$col.rm_data_var]] %in% input$level.rm_data_level,]
      
      # data <- data %>% filter( ! .data[[input$col.rm_data_var]] %in% input$level.rm_data_level )
      
    }
    
    # A linha a seguir sera para remover uma ou mais colunas
    
    # se o usuario nao selecionar nada, uma coluna vazia e definida como nula,
    # ou seja, nao muda nada no dado.
    # por isso nao e necessario utilizar condicionais nesse caso
    
    data[, input$col.rm_vars] <- NULL

    # A seguir e feito o calculo do volume, caso o usuario nao insira uma variavel de volume e as variaveis necessarias para o calculo
    if( is.null(input$modelo_estvol) ||  is.null(input$col.dap)  || is.null(input$b0_estvol) || is.null(input$b1_estvol) || is.na(input$modelo_estvol) ||  is.na(input$col.dap)  || is.na(input$b0_estvol) || is.na(input$b1_estvol) || input$modelo_estvol =="" || input$col.dap ==""  || input$b0_estvol == "" || input$b1_estvol == ""  ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
    }else{
      
      if(input$modelo_estvol == "LN(VFFC) = b0 + b1 * 1/DAP + e"){
        data$VOL <- exp( input$b0_estvol + 1/data[[input$col.dap]] * input$b1_estvol )
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFFC = b0 + b1 * DAP + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]] * input$b1_estvol
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFFC = b0 + b1 * DAP² + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]]^2 * input$b1_estvol
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFFC = b0 + b1 * DAP + b2 * DAP² + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]] * input$b1_estvol + data[[input$col.dap]]^2 * input$b2_estvol
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFFC = b0 + b1 * LN(DAP) + e"){
        data$VOL <- input$b0_estvol + log(data[[input$col.dap]]) * input$b1_estvol
        data <- data %>% select(VOL, everything())
        
      }
      
      
      # modelos com b2 e ht precisam de mais uma condicao
      if( is.null(input$modelo_estvol) ||  is.null(input$col.ht)  |  is.na(input$col.ht) || is.na(input$b2_estvol) || input$col.ht ==""  || input$b2_estvol == "" ){
        
      }else if(input$modelo_estvol == "LN(VFFC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e"){
        data$VOL <- exp( input$b0_estvol + log(data[[input$col.dap]]) * input$b1_estvol + log(data[[input$col.ht]]) * input$b2_estvol )
        data <- data %>% select(VOL, everything())
        
      }else  if(input$modelo_estvol == "VFFC = b0 + b1 * DAP + b2 * HT + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]] * input$b1_estvol + data[[input$col.ht]] * input$b2_estvol
        data <- data %>% select(VOL, everything())
      }
      
      
    }
    
    # A seguir e feito o calculo da estrutura vertical, caso o usuario nao tenha inserido uma variavel referente a mesma, e selecione que desja calcular
    if(!is.null(input$est.vert.calc) && !is.na(input$est.vert.calc) && input$est.vert.calc=="Sim"){
      
      data <- estrat_vert_souza(data, input$col.ht)
      
    }
    
    data
    
  })
  
  # render
  output$prep_table <- renderDataTable({
    
    validate(need(!is.null(rawData()), "Please import a dataset"))
    
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
  
  # Set names
  
  varnames <- reactive({
    
    #req(input$col.especies,input$col.parcelas, input$col.dap,input$col.ht,input$col.vcc, input$col.vsc,input$col.area.parcela,input$col.area.total, input$col.col.agrup,  input$col.est.vertical,input$col.est.interna)
    
    varnameslist <- list(
      especies=input$col.especies,
      parcelas=input$col.parcelas,
      dap=input$col.dap,
      ht=input$col.ht,
      vcc=input$col.vcc,
      vsc=input$col.vsc,
      area.parcela=input$col.area.parcela,
      area.total=input$col.area.total,
      agrup=input$col.col.agrup,
      est.vertical=input$col.est.vertical,
      est.interna=input$col.est.interna
      )
    
    if(is.null(input$num.area.parcela)|| is.na(input$num.area.parcela) ||input$num.area.parcela==""){}else{varnameslist$area.parcela <- input$num.area.parcela  }
    if(is.null(input$num.area.total) || is.na(input$num.area.total) ||input$num.area.total==""){}else{varnameslist$area.total <- input$num.area.total  }
    
    if( !is.null(input$b0_estvol) && !is.na(input$b0_estvol) && !is.null(input$b1_estvol) && !is.na(input$b1_estvol)  ){
      varnameslist$vcc <- "VOL"
      }
    
    if(!is.null(input$est.vert.calc) && !is.na(input$est.vert.calc) && input$est.vert.calc=="Sim"){
      varnameslist$est.vertical <- "est.vert"
      }
    
    
    # Os nomes nao selecionados serao salvos como NULL na lista,
    # estes sao entao convertidos para "", por conveniencia 
    #x <- data.frame(do.call(cbind, lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )  ))    

    x <- lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )   
    x
  })
  
  output$teste <- renderTable({
    #print(varnames())
    varnames()
    
  })
  
})

