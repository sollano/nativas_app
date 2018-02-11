estrat_vert_souza <- function(df, ht ){
  
  
  
  df$est.vert <- ifelse(df[[ht]] < (mean(df[[ht]], na.rm=T) - sd(df[[ht]], na.rm=T) ) , "Inferior", 
                   ifelse(df[[ht]] >= (mean(df[[ht]], na.rm=T) - sd(df[[ht]], na.rm=T) ) & df[[ht]] < (mean(df[[ht]], na.rm=T) + sd(df[[ht]], na.rm=T) ), "Médio", 
                          ifelse(df[[ht]] >= (mean(df[[ht]], na.rm=T) + sd(df[[ht]], na.rm=T) ) , "Superior", NA
                          )
                   ) 
  )
  
  return(df)
  
}


estrat_vert_souza <- function(df, ht ){
  
  df %>%
    mutate(est.vert = case_when(
      
      .data[[ht]] < (mean(.data[[ht]], na.rm=T) - sd(.data[[ht]], na.rm=T) )               ~ "Inferior",
      .data[[ht]] >= (mean(.data[[ht]], na.rm=T) - sd(.data[[ht]], na.rm=T) ) & 
                    .data[[ht]] < (mean(.data[[ht]], na.rm=T) + sd(.data[[ht]], na.rm=T) ) ~ "Médio",
      .data[[ht]] >= (mean(.data[[ht]], na.rm=T) + sd(.data[[ht]], na.rm=T) )              ~ "Superior"

      
      
    )) %>% 
    select(est.vert, everything() )
  
}
