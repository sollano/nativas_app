fund_dap <- function(df, dap, .sep){
  
  dap_sym <- rlang::sym(gsub('"', "", deparse(substitute( dap ))))
  
  dap_old <- df %>% pull(!!dap_sym)
  
  list_split <- strsplit(dap_old,  .sep, fixed=T)
  list_split <- sapply(list_split, gsub, pattern=",",replacement= ".")
  list_split <- sapply(list_split, as.numeric)
  
  n.obs <- sapply(list_split, length) 
  seq.max <- seq_len(max(n.obs))
  
  df_split <- as.data.frame( t(sapply(list_split, "[", i = seq.max))  )
  df_split
  
  df$dap <- sqrt(rowSums(df_split^2, na.rm=T) )

  return(df)  
  
}
