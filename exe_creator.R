#install.packages("RInno")
devtools::install_github("ficonsulting/RInno")
require(RInno)
#RInno::install_inno()

create_app(
  app_name = "nativas_app", 
  app_dir  = "C:/Users/solla/Documents/R/trabalhos_manejo/nativas_app",
  pkgs     = c("shiny", "DT", "formattable", "readxl", "plyr", "dplyr", "ggplot2", "lazyeval", "ggdendro","ggthemes","xlsx")
  )
compile_iss()
