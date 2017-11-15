#' @export

fuste_summary <- function(df, arvore, fuste, dap, .groups){
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) || is.null(df) || is.na(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se arvore nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(arvore) || is.null(arvore) || is.na(arvore) || arvore == "" || is.null(df[[arvore]]) ){  
    stop("arvore not set", call. = F) 
  }
  
  # se fuste nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(fuste) || is.null(fuste) || is.na(fuste) || fuste == "" || is.null(df[[fuste]]) ){  
    stop("fuste not set", call. = F) 
  }
  
  # se dap nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap == "" || is.null(df[[dap]]) ){  
    stop("dap not set", call. = F) 
  }
  
  # se .groups nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(.groups) || is.null(.groups) || is.na(.groups) || .groups == "" || is.null(df[.groups]) ){  
    
    .groups_final <- c(arvore, fuste)
    
  }else{
    
    .groups_final <- c(.groups, arvore, fuste)
    
  }
  
  dap <- rlang::sym(dap)
  
  df %>% 
    group_by_at(vars(.groups_final)) %>% 
    summarise(DAP = mean((!!dap), na.rm = TRUE)  ) %>% 
    drop_na(DAP) %>% 
    ungroup
  
}
