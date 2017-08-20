#' @export

classe_diametro <- function(df, dap, parcela, area_parcela, ic = 5, dapmin = 5, especies=NA, volume=NA, cc_to_column=F){
  
  # se df nao for fornecido, for igual "", nulo, ou  nao for dataframe, parar
  if(  missing(df) || df == "" || is.null(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se dap nao for fornecido ou for igual "", parar
  if(  missing(dap) || gsub('"', "", deparse(substitute(dap)))== "" ){ 
    stop("dap not set", call. = F) 
  }
  
  # se parcela nao for fornecido ou for igual "", parar
  if(  missing(parcela) || gsub('"', "", deparse(substitute(parcela)))== "" ){ 
    stop("parcela not set", call. = F) 
  }
  
  # se area_parcela nao for fornecido ou for igual "", parar
  if(  missing(area_parcela) || gsub('"', "", deparse(substitute(area_parcela)))== "" ){ 
    stop("area_parcela not set", call. = F) 
  }
  
  # ic precisa ser numerico e de tamanho 1
  if(!is.numeric(ic) || length(ic)>1){
    stop("ic must be a single number", call. = F)
  }
  
  # dapmin precisa ser numerico e de tamanho 1
  if(!is.numeric(dapmin) || length(dapmin)>1){
    stop("dapmin must be a single number", call. = F)
  }

  # Permitir se e nse como entrada de nomes de variaveis
  DAP <- rlang::sym(gsub('"', "", deparse(substitute( dap ))))
  PARCELA <- rlang::sym(gsub('"', "", deparse(substitute( parcela ))))
  AREA_PARCELA <- rlang::sym(gsub('"', "", deparse(substitute( area_parcela ))))
  ESPECIES <- gsub('"', "", deparse(substitute( especies )))
  VOLUME <- rlang::sym(gsub('"', "", deparse(substitute( volume ))))
  
  # Testar se PARCELA e um nome de variavel
  test <- try(df %>% pull(!!PARCELA), silent = T)
  
  if(is(test, "try-error")){
    # Se nao for, salvar em npar (provavelmente e o numero de parcelas)
    npar <- parcela
  }else{
    # Se PARCELA for um nome de variavel, obter o numero de parcelas e salvar em uma var    
    npar <- df %>% pull(!!PARCELA) %>% as.factor %>% nlevels
  }
  
  
  # Testar se AREA_PARCELA e um nome de variavel
  testap <- try(df %>% pull(!!AREA_PARCELA), silent = T)
  
  if(is(testap, "try-error")){
    # Se nao for, salvar em npar (provavelmente e o numero de parcelas)
    AREA_PARCELA <- area_parcela
  }else{
    # Se PARCELA for um nome de variavel, obter o numero de parcelas e salvar em uma var    
    AREA_PARCELA <- df %>% group_by(!!PARCELA) %>% summarise(AREA = mean(!!AREA_PARCELA)) %>% summarise(A = mean(AREA)) %>% pull(A) 
  }
  
  # Testar se ESPECIES e um nome de variavel
  testesp <- try(df %>% pull(!!ESPECIES), silent = T)
  
  # Se especie for fornecida, ela deve ser um nome de variavel
  if(is(testesp, "try-error") && !is.na(especies) ){
    
    stop("especies must be a variable name",call. = F)
  
  # Se nao for, sera criada uma variavel descartavel no dataframe, para que o group_by possa rodar
  }else if(is.na(especies) || is.null(especies) || especies==""){
    
    df$ESPECIES <- "yay"
    
    
  }
  
  testvol<- try(df %>% pull(!!VOLUME), silent = T)
  
  # Se volume for fornecida, ela deve ser um nome de variavel
  if(is(testvol, "try-error") && !is.na(volume) ){
    
    stop("volume must be a variable name",call. = F)
    
    # Se nao for, sera criada uma variavel descartavel no dataframe, para que o summarise possa rodar
  }else if(is.na(volume) || is.null(volume) || volume==""){
    
    df$vol <- 23
    
    VOLUME <- rlang::sym("vol")
    
  }
  
  df_final <- df %>% 
    filter(!is.na(!!DAP)) %>% # remover NA
    mutate(CC = ceiling((!!DAP)/ic) * ic - ic/2  ) %>% # Calcular Centro de classe
    filter(CC > dapmin) %>% # Remover classes menores que o dap minimo
    group_by_at(vars(ESPECIES, CC )) %>% # Agrupar e calcular o numero de individuos, e n de individuos por ha
    summarise(
      NumIndv=n(),
      IndvHA = round( NumIndv / (AREA_PARCELA/10000 * npar ),  1 ),
      volume = sum( !!VOLUME  ) ) %>% 
    mutate(DR =  round(NumIndv/sum(NumIndv) * 100, 4) ) %>% # Calcular densidade relativa
    arrange( CC ) %>% 
    ungroup

  # Remover a variavel descartavel, caso tenha sido criada
  df_final$ESPECIES <- NULL
  
  # Remover VOLUME caso nao tenha fornecido pelo usuario
  if(is.na(volume) || is.null(volume) || volume==""){
    
    df_final$volume <- NULL
    
  }
  
  # Se o usuario quiser o centro de classe na coluna e nao tiver fornecido volume,
  # popular o centro de classe com o numero de individuos
  if(cc_to_column==T && !is(testesp, "try-error") && is(testvol, "try-error") ){
    
    df_final <- df_final %>% 
      select(!!ESPECIES,CC,NumIndv) %>% 
      spread(CC,NumIndv, fill = 0) 
   
    # Se o usuario quiser o centro de classe na coluna e tiver fornecido volume,
    # popular o centro de classe com o volume
  }else if(cc_to_column==T && !is(testesp, "try-error") && !is(testvol, "try-error") ){
    
    df_final <- df_final %>% 
      select(!!ESPECIES,CC,volume) %>% 
      tidyr::spread(CC,volume, fill = 0) 
    
  }else if(cc_to_column==T && is(testesp, "try-error") ){
    
    stop("Especies column must be provided if cc_to_column is true ", call. = F)
    
  }
  
  return(df_final)

}
