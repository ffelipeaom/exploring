install.packages("tidyverse")
library("tidyverse")
library(readxl)
library(janitor)

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

df <- read.csv("despesa_ceaps_2022.csv", header = FALSE, sep = ";", dec = ".")
names(df)

df <- row_to_names(df, row_number = 2, remove_rows_above = TRUE)
names(df)
glimpse(df)
#head(df)
#summary(df)
#str(df)
#count(df, name = 'DOCUMENTO')
#unique(df$`DOCUMENTO`)
#apply(df_na,2, function(x) is.na(x))
apply(df,2,function(x) sum(is.na(x)))

df_na <- df
df_na[df_na == "" | df_na == " "] <- NA
#df <- df %>% mutate(DETALHAMENTO = na_if(DETALHAMENTO, ""))

apply(df_na,2,function(x) sum(is.na(x)))
sum(!complete.cases(df_na))
glimpse(df_na)

df_na <- relocate(df_na, COD_DOCUMENTO)
df_date <- select(df_na, - c(ANO, MES))

