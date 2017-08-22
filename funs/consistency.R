consistency <- function(df, dap, ht, parcela,lower=0.2, upper=10){
  
  DF <- as.data.frame(df)
  
  # se DF nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(DF) || is.null(DF) || is.na(DF) || !is.data.frame(DF) ){  
    stop("DF not set", call. = F) 
  }
  
  # se dap nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap == "" || is.null(DF[[dap]] ) ){  
    stop("dap not set", call. = F) 
  }
  
  # se ht nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(ht) || is.null(ht) || is.na(ht) || ht == "" || is.null(DF[[ht]] ) ){  
    stop("ht not set", call. = F) 
  }
  
  # add nomes das linhas como coluna numerica (numerica e importante caso se queira usar esta coluna como filtro futuramente)
  DF$rowid <- as.numeric(rownames(DF))
  
  
  if(missing(parcela) || is.null(parcela) || is.na(parcela) || parcela == ""){
    
    DF$parcela <- "grupoinuteel"
    PARCC <- "parcela"
  }else{
    
    PARCC <- parcela
    
  }
  
  DAPname <- dap
  HTname <- ht
  
  DF$rowid <- as.numeric(rownames(DF))
  
  y <-  DF %>% 
    group_by_at(vars(PARCC)) %>% 
    summarise_at(vars(DAPname, HTname), funs(mean(.,na.rm=T), sd(.,na.rm=T) ) ) %>% 
    full_join(DF,by=PARCC) %>% 
    mutate(
      DAP_test = ifelse(.data[[DAPname]] <=  .data[[paste(DAPname,"mean",sep="_") ]] -.data[[paste(DAPname,"sd",sep="_") ]]*3, "DAP menor que media - 3 sd", ifelse(.data[[DAPname]] >  .data[[paste(DAPname,"mean",sep="_") ]] + .data[[paste(DAPname,"sd",sep="_") ]]*3, "DAP maior que media + 3 sd", "ok" )  ),
      HT_test = ifelse(.data[[HTname]]  <=  .data[[paste(HTname,"mean",sep="_") ]] - .data[[paste(HTname,"sd",sep="_") ]]*3, "Altura menor que media - 3 sd", ifelse(.data[[HTname]]  >  .data[[paste(HTname,"mean",sep="_") ]] + .data[[paste(HTname,"sd",sep="_") ]]*3, "Altura maior que media + 3 sd", ifelse(is.na( .data[[DAPname]] ) & is.numeric( .data[[HTname]] ), "Arvore com altura e sem dap", "ok")       )  ),
      ratio_test =  ifelse(.data[[DAPname]]/.data[[HTname]] <= lower | .data[[DAPname]]/.data[[HTname]] >= upper , "Razao dap/ht ruim", "ok"  )  
    ) %>% 
    filter( !is.na(.data[[DAPname]]) & !is.na(.data[[HTname]])  ) %>% 
    filter(DAP_test != "ok" | HT_test != "ok" | ratio_test != "ok") %>% 
   # filter_at(vars( "DAP_test", "HT_test", "ratio_test" ), any_vars(. != "ok") ) %>% 
    select(rowid, DAP_test, HT_test, ratio_test,  everything() ) %>% 
    arrange(rowid)
  
  if(missing(parcela) || is.null(parcela) || is.na(parcela) || parcela == ""){
    
    DF$parcela <- NULL
  }
  
  if(nrow(y) == 0){
    z <- NULL
    # warning("No inconsistencies were found. yay!",call. = F)
  }else{
    z <- y
  }
  
  # retorna um datafram com as linhas que se destacaram na razao
  return( z )
  
  
}
