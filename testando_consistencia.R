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

