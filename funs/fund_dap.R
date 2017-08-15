fund_dap <- function(df, dap, .sep){
  
  dap_sym <- rlang::sym(gsub('"', "", deparse(substitute( dap ))))
  
  # define a variavel como um objeto separado
  dap_old <- df %>% pull(!!dap_sym)
  
  
  # separa os daps de acordo com .sep, e salva em uma lista
  list_split <- strsplit(dap_old,  .sep, fixed=T)
  
  # Substitui "," por ".", caso ocorra. Isso ia ocorrer caso o
  # computador de onde o arquivo originou seja brasileiro.
  # Os valores separados por "+" nao serao importados como numero,
  # com isso, os numeros serao salvos com o decimal ",". Para converte-los
  # novamente a numero, deve-se primeiro substituir a virgula por ponto.
  list_split <- sapply(list_split, gsub, pattern=",",replacement= ".")
  
  # Converter valores convertidos para numero
  list_split <- sapply(list_split, as.numeric)
  
  # Descobrir o numero maximo de dametros medidos
  # e salvar em um objeto
  n.obs <- sapply(list_split, length) 
  
  # Cria uma sequencia de numeros com base em n.obs
  # ex: se n.obs for 3, seq.max sera um vetor c(1,2,3)
  seq.max <- seq_len(max(n.obs))
  
  # sapply combinado com "[" ira criar um vetor 
  # com seq.max linhas e dap_old colunas.
  # t() ira transpor isto, deixando cada dap em uma coluna.
  # se seq.max for 3, arvores com dap fundido menor que 3 terao
  # NA como espaco vazio (essa e uma propriedade de "[" )
  df_split <- as.data.frame( t(sapply(list_split, "[", i = seq.max))  )
  df_split
  
  # Por fim realiza-se a raiz da soma das linhas ao quadrado, removendo NA
  df$dap <- sqrt(rowSums(df_split^2, na.rm=T) )

  return(df)  
  
}
