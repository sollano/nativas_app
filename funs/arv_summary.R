#' @export

arv_summary <- function(df, arvore, dap, .groups, area_parcela, area_total, ht, vol){
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) || is.null(df) || is.na(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se arvore nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(arvore) || is.null(arvore) || is.na(arvore) || arvore == "" || is.null(df[[arvore]]) ){  
    stop("arvore not set", call. = F) 
  }
  
  # se dap nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap == "" || is.null(df[[dap]]) ){  
    stop("dap not set", call. = F) 
  }
  
  # se .groups nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  # all garante que varios resultados possam ser testados,e somente se todos forem vazios ou na
  # que a funcao ira parar
  if(  missing(.groups) || all(is.null(.groups)) || all(is.na(.groups)) || all(.groups == "") ){  
    
    .groups_final <- arvore
  }else{
    
    # comando para remover grupos vazios ("" NA to null )
    .groups <- do.call(cbind, lapply(.groups, function(x){if(is.na(x)|x==""){x<-NULL}else{x} } ) )   
    
    .groups_final <- c(.groups, arvore)
  }
  
  # Variaveis opcionais
  if(missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) || area_parcela==F || area_parcela==""   ){df$area_parcela_calc<-NA; area_parcela_calc <- "area_parcela_calc"}else{area_parcela_calc <- area_parcela}
  if(missing(area_total) || is.null(area_total) || is.na(area_total) || area_total==F || area_total==""   ){df$area_total_calc<-NA; area_total_calc <- "area_total_calc"}else{area_total_calc <- area_total}
  
  if(missing(vol)  || is.null(vol)  || is.na(vol) || vol==F|| vol=="" ){df$vol_calc<-NA; vol_calc <- "vol_calc"}else(vol_calc <- vol)
  if(missing(ht)   || is.null(ht)  || is.na(ht) || ht==F || ht=="" ){df$ht_calc<-NA; ht_calc <- "ht_calc"}else(ht_calc <- ht)
  
  # argumentos de area podem ser numericos
  # se nao colocar esse if missing vazio aqui, da erro,
  # por causa do is.numeric com argumento vazio
  if(missing(area_parcela)){}else if(is.numeric(area_parcela)){df$area_parcela_calc <- area_parcela; area_parcela_calc <- "area_parcela_calc"}
  if(missing(area_total)){}else if(is.numeric(area_total  )){df$area_total_calc   <- area_total; area_total_calc     <- "area_total_calc"}
  
  area_parcela_calc <- rlang::sym(area_parcela_calc)
  area_total_calc <- rlang::sym(area_total_calc)
  dap <- rlang::sym(dap)
  ht_calc <- rlang::sym(ht_calc)
  vol_calc <- rlang::sym(vol_calc)
  
  y <- df %>% 
    group_by_at(vars(.groups_final)) %>% 
    summarise(
      AREA_PARCELA = mean((!!area_parcela_calc), na.rm = TRUE),
      AREA_TOTAL = mean((!!area_total_calc), na.rm = TRUE),
      DAP = sqrt( sum( (!!dap)^2, na.rm=T) ),
      HT  = mean((!!ht_calc), na.rm = TRUE),
      VOL = mean((!!vol_calc), na.rm = TRUE)
    ) %>% 
    na_if(0) %>% 
    as.data.frame
  ungroup
  
  # Remover variaveis nao informadas
  if(missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) || area_parcela==F || area_parcela==""   ){y$AREA_PARCELA <- NULL}
  if(missing(area_total) || is.null(area_total) || is.na(area_total) || area_total==F || area_total==""   ){y$AREA_TOTAL <- NULL}
  
  if(missing(vol)  || is.null(vol)  || is.na(vol) || vol==F|| vol=="" ){y$VOL <- NULL}
  if(missing(ht)   || is.null(ht)  || is.na(ht) || ht==F || ht=="" ){y$HT <- NULL}
  
  
  
  return(y)
}
