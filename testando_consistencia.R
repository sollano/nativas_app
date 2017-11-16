# testando a funcao de consistencia ####
df <- readxl::read_xlsx("examples/Inventory_examplo.xlsx", sheet = 2)

df <- read.csv("examples/Inventory_exemplo_consistido.csv")

#dfrazao <- consistency(df, "DAP", "HT", "transecto")
dfrazao <- consistency(df, "DAP", "HT", "transecto")
dfrazao$rowid
write.csv(df[-dfrazao$rowid, ], file = "examples/Inventory_exemplo_consistido.csv", row.names = F)


#htdapratio(read.csv("examples/Inventory_exemplo_consistido.csv"), "DAP", "HT")
consistency(read.csv("examples/Inventory_exemplo_consistido.csv"), "DAP", "HT", "transecto")
#  #####

ex_fuste %>% pull(Especie) %>% as.factor %>% levels


ex_fuste %>% 
  mutate( teste_especie = case_when(
    
    Especie ==""                      ~ "Especie vazia",
    stringr::str_sub(Especie, 1)==" " ~ "Espaco vazio no inicio da especie", 
    stringr::str_sub(Especie,-1)==" " ~ "Espaco vazio no final da especie",
    TRUE                              ~ "ok"
    
  ) )

ex_fuste %>% 
  group_by(Especie) %>% 
  summarise(N=n()) %>% 
  mutate( especie_test = case_when(
    
    Especie ==""                      ~ "Especie vazia",
    stringr::str_sub(Especie, 1)==" " ~ "Espaco vazio no inicio da especie", 
    stringr::str_sub(Especie,-1)==" " ~ "Espaco vazio no final da especie",
    TRUE                              ~ "ok"
    
  ) ) %>% 
  filter(especie_test != "ok")


consistency(df,dap =  "DAP",ht =  "HT", parcela = "transecto", especie = "nome.cient") %>% head
consistency(ex_fuste,cap =  "CAP",ht = "HT",parcela =  "Parcela",especie =  "Especie")# %>% head
consistency(ex_fuste,cap =  "CAP")# %>% head
consistency(ex_fuste,cap =  "CAP",especie = "Especie")# %>% head

ex_fuste %>% pull(Especie) %>% factor %>% levels
