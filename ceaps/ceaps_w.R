install.packages("tidyverse")
library("tidyverse")
library("readxl")
library("janitor")
library("knitr")
library("tmap")

#identificar  campos que possuem valores nulos ou duplicados
#converter campos de data que estão sendo carregados como texto
#corrigir valores monetários
#nomes incorretos
#formatar campos de CNPJ

#lidar com dados nulos (deletar ou imputar um valor novo)
#remover colunas que não trazem nenhuma informação
#processar datas que estão em formato incorreto
#alterar o tipo da coluna
#remover duplicados

#remove(df)

ceaps22 <- read.csv("despesa_ceaps_2022.csv", header = FALSE, sep = ";", dec = ".")
ceaps21 <- read.csv("despesa_ceaps_2021.csv", header = FALSE, sep = ";", dec = ".")
ceaps20 <- read.csv("despesa_ceaps_2020.csv", header = FALSE, sep = ";", dec = ".")
ceaps19 <- read.csv("despesa_ceaps_2019.csv", header = FALSE, sep = ";", dec = ".")
ceaps18 <- read.csv("despesa_ceaps_2018.csv", header = FALSE, sep = ";", dec = ".")

View(ceaps22)
View(ceaps21)
View(ceaps20)
View(ceaps19)
View(ceaps18)
#ceaps22 %>% row_to_names(row_number = 2, remove_rows_above = TRUE)
#ceaps21 %>% row_to_names(row_number = 2, remove_rows_above = TRUE)
#ceaps20 %>% row_to_names(row_number = 2, remove_rows_above = TRUE)
#ceaps19 %>% row_to_names(row_number = 2, remove_rows_above = TRUE)
#ceaps18 %>% row_to_names(row_number = 2, remove_rows_above = TRUE)

tmap_mode("view")
ceaps22 %>% kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

names(ceaps22)
glimpse(ceaps22)
head(ceaps22)
summary(ceaps22)
str(ceaps22)

#counting NA
apply(ceaps22,2,function(x) sum(is.na(x))) #by column
sum(!complete.cases(ceaps22)) #total

#listing dataframes
df.list <- list(ceaps22, ceaps21, ceaps20, ceaps19, ceaps18)
names(df.list) <- c("ceaps22", "ceaps21", "ceaps20", "ceaps19", "ceaps18")
names(df.list)

for (i in 1:length(df.list)) {
  glimpse(df.list[[i]])
}

#counting NA for all dataframes
for (i in 1:length(df.list)) {
  print(sum(!complete.cases(df.list[[i]])))
}

#treating dataframes
for (i in 1:length(df.list)) {
  df.list[[i]] <- row_to_names(df.list[[i]], row_number = 2, remove_rows_above = TRUE)
  df.list[[i]][df.list[[i]] == "" | df.list[[i]] == " "] <- NA
  df.list[[i]] <- relocate(df.list[[i]], COD_DOCUMENTO)
  df.list[[i]] <- select(df.list[[i]], - c(ANO, MES))
  print(sum(!complete.cases(df.list[[i]])))
  #TODO: convert types
}
glimpse(df.list[[1]])
list2env(df.list, .GlobalEnv) #brings back each df to global environment

count(ceaps22, name = 'DOCUMENTO')
unique(ceaps18$`DOCUMENTO`)

##---
#df1 <- read.csv("despesa_ceaps_2022.csv", header = FALSE, sep = ";", dec = ".")
#df2 <- read.csv("despesa_ceaps_2021.csv", header = FALSE, sep = ";", dec = ".")
#df3 <- read.csv("despesa_ceaps_2020.csv", header = FALSE, sep = ";", dec = ".")
#df4 <- read.csv("despesa_ceaps_2019.csv", header = FALSE, sep = ";", dec = ".")
#df5 <- read.csv("despesa_ceaps_2018.csv", header = FALSE, sep = ";", dec = ".")

#In order to store all datasets in one list using their name
#df.list <- lapply(1:5, function(x) eval(parse(text=paste0("df", x))))
#Adding the name of each df in case you want to unlist the list afterwards
#names(df.list) <- lapply(1:5, function(x) paste0("df", x))
#for (i in 1:length(df.list)) {
#  df.list[[i]] <- row_to_names(df.list[[i]], row_number = 2, remove_rows_above = TRUE)
#    #rowMeans(df.list[[i]][1:2])
#}
#list2env(df.list,.GlobalEnv)
##---

#names(df)
#glimpse(df)
#head(df)
#summary(df)
#str(df)
#count(df, name = 'DOCUMENTO')
#unique(df$`DOCUMENTO`)
#apply(df_na,2, function(x) is.na(x))


#df <- ceaps22
#df_na <- df
#df_na[df_na == "" | df_na == " "] <- NA
#df <- df %>% mutate(DETALHAMENTO = na_if(DETALHAMENTO, ""))
#glimpse(df_na)
#df_na <- relocate(df_na, COD_DOCUMENTO)
#df_date <- select(df_na, - c(ANO, MES))












