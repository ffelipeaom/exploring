library("dplyr")
library("tidytext")
library("tidyr")
library("rwhatsapp")
library("lubridate")
library("stopwords")

#load files
files <- list.files(pattern = "^[C]")
data <- list() #creating empty list

#read data
for (i in seq_along(files)) {
  data <- lapply(files, rwa_read)
}

#define messenger receiver to later exclude our own messages
receiver <- "Felipe Augusto" 

#filters messages from clients only, from all data loaded
for (i in seq_along(data)) {
  d <- data[[i]]
  sender <- d$author[(d$author!=receiver) & !is.na(d$author)][1] %>% toString()

    d <- d %>% select(time, author, text) %>%
    drop_na() %>%
    filter(text!="<Arquivo de mídia oculto>" & author!=receiver) %>% 
    unnest_tokens(input = text, output = word, token = "words")
    #%>% count(author, word, sort = TRUE)
    #%>% rename_with(.cols = text, ~sender) #keeping author col can be useful
    #%>% select(-author)
  data[[i]] <- d
}
View(data[[2]])
View(data[[1]])

to_remove <- c(stopwords(language = "pt"), "pra", "https", "é", "eh")
m <- data[[2]] %>% filter(!word %in% to_remove)

#p <- tibble(txt = data[[2]])
t <- data[[1]]
t

t %>% unnest_tokens(input = Marezis, output = word) %>%
  count(word, sort = TRUE)



##TODO
#tokenize
#using time as messenger identifier, find tokens of interest
#(like "rua", "avenida" for address, series of numbers for telephone)
#find and save such messages into a df with respective client
#(client name is the sender, name of column of each worked df in data