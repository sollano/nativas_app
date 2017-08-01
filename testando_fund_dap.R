library(readxl)
df <- read_excel("C:/Users/User/Downloads/DAP Fundido (1).xlsx", 2)
df <- as.data.frame(df)
head(df,20)
cSplit(df, "DAP", "+")
df[[11,2]]
strsplit(df[[11,2]],  "+", fixed=T)->a

b <- as.data.frame(t(strsplit(df[[,2]],  "+", fixed=T)[[1]]) )
b
gsub(",", ".",b[[2]])

sub("\\.", ",", strsplit(df[[11,2]],  "+", fixed=T) )



# ####

list_split <- strsplit(df[,2],  "+", fixed=T)
list_split <- sapply(list_split, gsub, pattern=",",replacement= ".")
list_split <- sapply(list_split, as.numeric)

df_split <- do.call(bind_rows, sapply(list_split, function(x) {as.data.frame(t( x ))} ) )
df_split

df_split$DAP <- sqrt(rowSums(df_split^2, na.rm=T) )
df_split


# ####
list_split <- strsplit(df[,2],  "+", fixed=T)
list_split <- sapply(list_split, gsub, pattern=",",replacement= ".")
list_split <- sapply(list_split, as.numeric)

n.obs <- sapply(list_split, length) 
seq.max <- seq_len(max(n.obs))

df_split <- as.data.frame( t(sapply(list_split, "[", i = seq.max))  )
df_split
df_split$DAP <- sqrt(rowSums(df_split^2, na.rm=T) )
df_split

# the trick is, that,
# c(1:2)[1:4]
# returns the vector + two NAs
# e. g.
# list_split[[1]][c(1,2,3)]




fund_dap(df,"DAP", "+")
