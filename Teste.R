source("funs/diversidade.R")
source("funs/pareadoSimilaridade.R")
source("funs/matrizSimilaridade.R")
source("funs/estrutura.R")
source("funs/BDq.R")
source("funs/agregacao.R")
source("funs/inv_summary.R")

inv = read.csv("examples/cauaxi 2012.csv")
head(estrutura(inv, "scientific.name", "DBH", "transect", 1000)) # horizontal
head(estrutura(inv, "scientific.name", "DBH", "transect", 1000, "canopy")) # vertical
head(estrutura(inv, "scientific.name", "DBH", "transect", 1000, "canopy", "light")) # vertical + interna
diversidade(inv, "scientific.name", indice = "H")
diversidade(inv, "scientific.name", indice = "S")
diversidade(inv, "scientific.name", indice = "Hmax")
diversidade(inv, "scientific.name", indice = "J")
diversidade(inv, "scientific.name")
diversidade(inv, "scientific.name", "transect") # por parcela
p.similaridade(inv[inv$transect == "T01","scientific.name"], inv[inv$transect == "T02","scientific.name"])
m.similaridade(inv, "scientific.name", "transect")
bdq.meyer(inv, "transect", "DBH", 1000)[[1]]
agregacao(inv, "scientific.name", "transect")

inv = read.csv("examples/ducke_.csv")
estrutura(inv, "scientific_name", "DBH_11", "transect", 1000, "canopy_11", "light_11")
diversidade(inv, "scientific_name", indice = "H")
diversidade(inv, "scientific_name", indice = "S")
diversidade(inv, "scientific_name", indice = "Hmax")
diversidade(inv, "scientific_name", indice = "J")
diversidade(inv, "scientific_name")
diversidade(inv, "scientific_name", "transect") # por parcela
m.similaridade(inv, "scientific_name", "transect")
bdq.meyer(inv, "transect", "DBH_11", 1000)[1]
agregacao(inv, "scientific_name", "transect")

#inv = read.csv("nativas_app/examples/Inventory_exemplo.csv")
inv = read.csv("examples/Inventory_exemplo.csv")
inv2 <- readxl::read_excel("examples/Inventory_example.xlsx")
inv2 <- readxl::read_excel("examples/Inventory_example.xlsx")
inv2 <- data.frame(inv)

head(estrutura(inv, "nome.cient", "dap", "transecto", 10000)) # horizontal
head(estrutura(inv, "nome.cient", "dap", "transecto", "parc.area")) # horizontal
head(estrutura(inv, "nome.cient", "dap", "transecto", 10000, "pos.copa")) # vertical
head(estrutura(inv, "nome.cient", "dap", "transecto", 10000, "pos.copa", "luminosidade")) # vertical + interna
diverinsidade(inv, "nome.cient", indice = "H")
diversidade(inv, "nome.cient", indice = "S")
diversidade(inv, "nome.cient", indice = "Hmax")
diversidade(inv, "nome.cient", indice = "J")
diversidade(inv, "nome.cient")
diversidade(inv, "nome.cient", "transecto") # por parcela
p.similaridade(inv[inv$transecto == "T01","nome.cient"], inv[inv$transecto == "T02","nome.cient"])
m.similaridade(inv, "nome.cient", "transecto")
bdq.meyer(inv, "transecto", "dap", 10000)[[1]]
agregacao(inv, "nome.cient", "transecto")

# Totalizacao de arvores ####
library(dplyr)
inv <- read.csv2("examples/Inventory_exemplo_fuste.csv") %>% mutate(area_parcela = 450, area_total = 50)
head(inv)
# Sem informar area
head(arv_summary(df = inv,arvore = "NP",dap = "CAP", .groups = c("Parcela", "Especie") ), 10)

# informando area parcela numerico
head(arv_summary(df = inv,arvore = "NP",dap = "CAP", .groups = c("Parcela", "Especie"),area_parcela = 450), 10)

# informando area parcela e area total numerico
head(arv_summary(df = inv,arvore = "NP",dap = "CAP", .groups = c("Parcela", "Especie"),area_parcela = 450, area_total = 50), 10)

# informando area parcela variavel
head(arv_summary(df = inv,arvore = "NP",dap = "CAP", .groups = c("Parcela", "Especie"),area_parcela = "area_parcela"), 10)

# informando area parcela e area variavel
head(arv_summary(df = inv,arvore = "NP",dap = "CAP", .groups = c("Parcela", "Especie"),area_parcela = 450, area_total = 50), 10)


# Totalizacao de Parcelas ####

# calculo do volume e add da coluna de area (em m²)
library(dplyr)
library(lazyeval)

inv <- read.csv("examples/cauaxi 2012_.csv") %>% 
  mutate(
    VOL           = 0.000503 * DBH^2.187162,
    total.area    = 50,
    transect.area = 1000        )

#write.csv(inv, "cauaxi 2012_.csv", row.names = F)

# obs: a funcao a seguir requer as versoes mais recentes dos pacotes dplyr & lazyeval;
# A funcao inv_summary gera valores de dap, diametro quadratico e altura e altura dominante medias;
# e somas de area basal e volume;
# Melhor utilizada para passar tabelas em nivel de arvore para nivel de parcela;

# a altura dominante pode ser fornecida como nome de coluna pelo argumento "Hd"; 
# caso contrario, sera calculada com base nas duas maiores arvores;

# argumentos adicionais calculam a media da area total, media da idade, e media do volume sem casca
# outro argumento adicional e o "grupos".
# caso nao seja fornecido, ou seja falso, o sumario ira considerar o df como um todo
# caso ele seja fornecido, o inventario sera feito por n grupo(s)

# Inv geral, sem grupos
inv_summary(inv, "DBH","Htot", "VOL", "transect.area", groups = F)

# Inv geral, por parcela
inv_summary(inv,"DBH", "Htot", "VOL", "transect.area", groups = "transect")

# Inv geral, por especie
inv_summary(inv,"DBH", "Htot", "VOL", "transect.area", groups = "scientific.name")

# Inv geral, por parcela e especie
inv_summary(inv, "DBH", "Htot", "VOL", "transect.area", groups = c("transect", "scientific.name") )

# Argumento de area pode ser numerico
inv_summary(inv, "DBH", "Htot", "VOL", 1000, groups = c("transect", "scientific.name") )

# testando mensagens de erro 
inv_summary()
inv_summary(inv)
inv_summary(inv, "DBH")
inv_summary(inv, "DBH", "Htot")
inv_summary(inv, "DBH", "Htot", "VOL")
inv_summary(inv, "DBH", "Htot", "VOL", "transect.area")


inv2 <- read.csv("ducke.csv") %>% 
  mutate(
    VOL           = 0.000503 * DBH_11^2.187162,
    total.area    = 50,
    transect.area = 1000        )

# Inv geral, sem grupos
inv_summary(inv2, "DBH_11","Htot_11", "VOL", "transect.area", groups = F)

# Inv geral, por parcela
inv_summary(inv2,"DBH_11", "Htot_11", "VOL", "transect.area", groups = "transect")

# Inv geral, por parcela e especie
inv_summary(inv2, "DBH_11", "Htot_11", "VOL", "transect.area", groups = c("transect", "scientific.name") )

# Argumento de area pode ser numerico
inv_summary(inv2, "DBH_11", "Htot_11", "VOL", 1000, groups = c("transect", "scientific.name") )




# centro de classe ####
inv <- read.csv("examples/Inventory_exemplo.csv")

bdq.meyer(inv, "transecto", "dap", 10000, intervalo.classe = 5, min.dap = 5)[[1]]
classe_diametro(inv, "dap","transecto", 10000,ic = 5, dapmin = 5)
# diametro maximo do bdq.meyer esta errado

bdq.meyer(inv, "transecto", "dap", 10000, intervalo.classe = 10, min.dap = 5)[[1]]
classe_diametro(inv, "dap","transecto", 10000,ic = 10, dapmin = 5)

classe_diametro(inv, "dap","transecto", 10000,"nome.cient",ic = 10, dapmin = 5)

classe_diametro(inv, "dap","transecto", 10000,"nome.cient",ic = 10, dapmin = 5, cc_to_column = T)
classe_diametro(inv, "dap","transecto", 10000,"nome.cient",ic = 10, dapmin = 5, cc_to_column = T, G_to_cc = T)


# Gráfico IVI ####

gdata <- estrutura(inv, "nome.cient", "dap", "transecto", "parc.area") %>% 
  arrange(-IVI) %>% 
  mutate(n = as.numeric(row.names(.)), class = ifelse(n>10,"Demais especies",as.character(especie)),class = factor(class, levels=unique(class)) ) 

gdata$class

gdata2 <- 
  gdata %>% 
  gather(IVI_contrib, valor, FR , DR , DoR, factor_key = T) %>% 
  group_by(class, IVI_contrib) %>% 
  summarise(valor_d = sum(valor), IVI = sum(IVI), IVI_sep = valor_d/3,IVI_contrib_porc = round(valor_d/3/IVI,2))
gdata2

ggplot(gdata2, aes( ordered(class, levels = rev(levels(class)) ) , IVI_sep, fill=IVI_contrib ) ) + 
  geom_bar(stat = "identity", width = .8, color = "black") +
  # geom_text(aes(label = scales::percent(IVI_contrib_porc) ), position = position_stack(vjust = 0.5), size = 4) + 
  coord_flip() +
  labs(x = "Especies", y="IVI", fill = "Legenda") +
  ggthemes::theme_igray(base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size=22, face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title   = element_text(size = 26,face="bold"), 
    axis.text    = element_text(size = 22),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    strip.text.x = element_text(size = 22)   ) + 
  guides(fill = guide_legend(reverse=T))



estrutura(inv, "nome.cient", "dap", "transecto", "parc.area") %>% 
  arrange(-IVI) %>% 
  mutate(n = as.numeric(row.names(.)), class = ifelse(n>10,"Demais especies",as.character(especie)),class = factor(class, levels=unique(class)) )%>% 
  gather(IVI_contrib, valor, FR , DR , DoR, factor_key = T) %>% 
  group_by(class, IVI_contrib) %>% 
  summarise(valor_d = sum(valor), IVI = sum(IVI), IVI_sep = valor_d/3,IVI_contrib_porc = round(valor_d/3/IVI,2)) %>% 
  ggplot(aes( ordered(class, levels = rev(levels(class)) ) , IVI_sep, fill=IVI_contrib ) ) + 
  geom_bar(stat = "identity", width = .8, color = "black") +
  # geom_text(aes(label = scales::percent(IVI_contrib_porc) ), position = position_stack(vjust = 0.5), size = 4) + 
  coord_flip() +
  labs(x = "Especies", y="IVI", fill = "Legenda") +
  ggthemes::theme_igray(base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size=22, face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title   = element_text(size = 26,face="bold"), 
    axis.text    = element_text(size = 22),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    strip.text.x = element_text(size = 22)   ) + 
  guides(fill = guide_legend(reverse=T))

# Gráfico est vertical ####

g_est_vert <- estrat_vert_souza(inv, "Htot")# %>% 
  #group_by(est.vert) %>% 
 # summarise(Frequência = n() )

g_est_vert

ggplot(g_est_vert, aes(x=as.factor(est.vert) ) ) + 
  geom_bar(stat="count", color = "black") +
  scale_x_discrete(
    labels= c( 
      expression(bar(Ht) < S  ),
      expression(S <= bar(Ht), a >= S   ), 
      expression(bar(Ht) > S  )   )  ) + 
  # geom_text(aes(label = scales::percent(IVI_contrib_porc) ), position = position_stack(vjust = 0.5), size = 4) + 
  labs(x = "Estrutura vertical", y="Frequência") +
  ggthemes::theme_igray(base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size=22, face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title   = element_text(size = 26,face="bold"), 
    axis.text    = element_text(size = 22),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    strip.text.x = element_text(size = 22)   ) + 
  guides(fill = guide_legend(reverse=T))






