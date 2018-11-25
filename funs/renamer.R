renamer <- function(df, arvore,parcelas,especies,cap,dap,ht,
                    vcc,vsc,area.parcela,area.total,
                    est.vertical,est.interna,estrato){
  
  
  # ####
  
  dfmod <- data.frame()
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }else{
    df <- as.data.frame(df)
    dfmod <- data.frame(XX=1:nrow(df))
  }
  
  # se arvore nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(arvore) ){  
    
  }else if( !is.character(arvore) ){
  }else if(length(arvore)!=1){
    stop("Length of 'arvore' must be 1", call.=F)
  }else if(forestmangr::check_names(df, arvore)==F){
    stop(forestmangr::check_names(df, arvore, boolean=F), call.=F)
  }else{
    dfmod[["arvore"]] <- df[[arvore]]
  }
  
  # se parcelas nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  # se parcelas nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(parcelas) ){  
  }else if( !is.character(parcelas) ){
  }else if(length(parcelas)!=1){
    stop("Length of 'parcelas' must be 1", call.=F)
  }else if(forestmangr::check_names(df, parcelas)==F){
    stop(forestmangr::check_names(df, parcelas, boolean=F), call.=F)
  }else{
    dfmod[["parcelas"]] <- df[[parcelas]]
  }
  
  # se especies nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(especies) ){  
   
  }else if( !is.character(especies) ){
  }else if(length(especies)!=1){
    stop("Length of 'especies' must be 1", call.=F)
  }else if(forestmangr::check_names(df, especies)==F){
    stop(forestmangr::check_names(df, especies, boolean=F), call.=F)
  }else{
    dfmod[["especies"]] <- df[[especies]]
  }
  
  # se cap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(cap) ){  
    
  }else if( !is.character(cap) ){
  }else if(length(cap)!=1){
    stop("Length of 'cap' must be 1", call.=F)
  }else if(forestmangr::check_names(df, cap)==F){
    stop(forestmangr::check_names(df, cap, boolean=F), call.=F)
  }else{
    dfmod[["cap"]] <- df[[cap]]
  }
  
  # se dap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dap) ){  
    
  }else if( !is.character(dap) ){
  }else if(length(dap)!=1){
    stop("Length of 'dap' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dap)==F){
    stop(forestmangr::check_names(df, dap, boolean=F), call.=F)
  }else{
    dfmod[["dap"]] <- df[[dap]]
  }
  
  # se ht nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(ht) ){  
    
  }else if( !is.character(ht) ){
  }else if(length(ht)!=1){
    stop("Length of 'ht' must be 1", call.=F)
  }else if(forestmangr::check_names(df, ht)==F){
    stop(forestmangr::check_names(df, ht, boolean=F), call.=F)
  }else{
    dfmod[["ht"]] <- df[[ht]]
  }
  
  # se vcc nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(vcc) ){  
    
  }else if( !is.character(vcc) ){
  }else if(length(vcc)!=1){
    stop("Length of 'vcc' must be 1", call.=F)
  }else if(forestmangr::check_names(df, vcc)==F){
    stop(forestmangr::check_names(df, vcc, boolean=F), call.=F)
  }else{
    dfmod[["vcc"]] <- df[[vcc]]
  }
  
  # se vsc nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(vsc) ){  
    
  }else if( !is.character(vsc) ){
    
  }else if(length(vsc)!=1){
    stop("Length of 'vsc' must be 1", call.=F)
  }else if(forestmangr::check_names(df, vsc)==F){
    stop(forestmangr::check_names(df, vsc, boolean=F), call.=F)
  }else{
    dfmod[["vsc"]] <- df[[vsc]]
  }
  
  # se area.parcela nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area.parcela) ){  
    
  }else if( is.null(area.parcela) || is.na(area.parcela) || area.parcela == "" ){
  
  }else if(is.numeric(area.parcela) & length(area.parcela)==1){
    
  dfmod[["area.parcela"]] <- area.parcela
    
  }else if(!is.character(area.parcela)){

  }else if(length(area.parcela)!=1){
    stop("Length of 'area.parcela' must be 1", call.=F)
  }else if(forestmangr::check_names(df, area.parcela)==F){
    stop(forestmangr::check_names(df, area.parcela, boolean = F), call.=F)
  }else{
    dfmod[["area.parcela"]] <- df[[area.parcela]]
  }
  
  # se area.total nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area.total) ){  
  }else if( is.null(area.total) || is.na(area.total) || area.total == "" ){
  }else if(is.numeric(area.total) & length(area.total)==1){

  dfmod[["area.total"]] <- area.total
  
  }else if(!is.character(area.total)){
  }else if(length(area.total)!=1){
    stop("Length of 'area.total' must be 1", call.=F)
  }else if(forestmangr::check_names(df, area.total)==F){
    stop(forestmangr::check_names(df, area.total, boolean = F), call.=F)
  }else{
    dfmod[["area.total"]] <- df[[area.total]]
  }
  
  # se est.vertical nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(est.vertical) ){  
  }else if( !is.character(est.vertical) ){
  }else if(length(est.vertical)!=1){
    stop("Length of 'est.vertical' must be 1", call.=F)
  }else if(forestmangr::check_names(df, est.vertical)==F){
    stop(forestmangr::check_names(df, est.vertical, boolean=F), call.=F)
  }else{
    dfmod[["est.vertical"]] <- df[[est.vertical]]
  }
  
  # se est.interna nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(est.interna) ){  
  }else if( !is.character(est.interna) ){
  }else if(length(est.interna)!=1){
    stop("Length of 'est.interna' must be 1", call.=F)
  }else if(forestmangr::check_names(df, est.interna)==F){
    stop(forestmangr::check_names(df, est.interna, boolean=F), call.=F)
  }else{
    dfmod[["est.interna"]] <- df[[est.interna]]
  }
  
  # se estrato nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(estrato) ){  
  }else if( !is.character(estrato) ){
  }else if(length(estrato)!=1){
    stop("Length of 'estrato' must be 1", call.=F)
  }else if(forestmangr::check_names(df, estrato)==F){
    stop(forestmangr::check_names(df, estrato, boolean=F), call.=F)
  }else{
    dfmod[["estrato"]] <- df[[estrato]]
  }
  # ####
  dfmod$XX <- NULL
  return(dfmod)
  
}