#' @export

classe_diametro <- function(df, dap, parcela, area_parcela, ic = 5, dapmin = 5){
  
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

  dap <- rlang::sym(gsub('"', "", deparse(substitute( dap ))))
  PARCELA <- rlang::sym(gsub('"', "", deparse(substitute( parcela ))))
  AREA_PARCELA <- rlang::sym(gsub('"', "", deparse(substitute( area_parcela ))))
  
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
  test <- try(df %>% pull(!!AREA_PARCELA), silent = T)
  
  if(is(test, "try-error")){
    # Se nao for, salvar em npar (provavelmente e o numero de parcelas)
    AREA_PARCELA <- area_parcela
  }else{
    # Se PARCELA for um nome de variavel, obter o numero de parcelas e salvar em uma var    
    AREA_PARCELA <- df %>% group_by(!!PARCELA) %>% summarise(AREA = mean(!!AREA_PARCELA)) %>% summarise(A = mean(AREA)) %>% pull(A) 
  }
  
  
  
  df %>% 
    filter(!is.na(!!dap)) %>% # remover NA
    mutate(CC = ceiling((!!dap)/ic) * ic - ic/2  ) %>% # Calcular Centro de classe
    filter(CC > dapmin) %>% # Remover classes menores que o dap minimo
    group_by(CC) %>% # Agrupar e calcular o numero de individuos, e n de individuos por ha
    summarise(NumIndv=n(),IndvHA = round( NumIndv / (AREA_PARCELA/10000 * npar ),  1 )   ) %>% 
    mutate(DR =  round(NumIndv/sum(NumIndv) * 100, 4) ) # Calcular densidade relativa
  
}
