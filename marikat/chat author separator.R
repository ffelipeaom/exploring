install.packages("ggimage")
library("rwhatsapp")
library("dplyr")
library("tidytext")
library("tibble")
library("tidyr")
library("ggplot2"); theme_set(theme_minimal())
library("ggimage")
#library("lubridate")


chat <- rwa_read("Conversa do WhatsApp com Maki.txt")
#Encoding(chat$text) <- "UTF-8"
View(chat)

chat_text <- chat[-c(1),] %>% select(author, text)
View(chat_text)
#chat_text[1,1]

q <- chat %>% unnest_tokens(input = text, output = word) %>%
  count(author, word, sort = TRUE)

#get client text
author_list <- unique(chat_text$author)
#author1 <- filter(chat_text, author == author_list[1]) %>% select(text)
client_text <- filter(chat_text, author == author_list[2]) %>% as.data.frame()# %>% select(text)


##finds and plot most used emojis per author
emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s.*", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))
#plot 
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = max(n), image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
##




#author1 %>% select(text)
##
#p <- data.frame(filter(chat_text, author == "Felipe Augusto"))
#for(i in author_list){
#  print(i)
  #o <- data.frame(filter(chat_text, author == i))
#  q <- assign(paste("author", length(author_list), sep = ""), author_list[[i]])
  
  #assign(paste("orca", i, sep = ""), list_name[[i]])
  #assign(paste0("author", i), author_list[[i]])
  #author <- lapply(unique(chat_text$author), function (x) d + rnorm (3))
  #eval(parse(text = paste('a', sep='')[i]))
  
  #author = filter(chat_text, author == i)
#}

chat_text_unnested <- chat_text %>% unnest_tokens(word, text)
View(chat_text_unnested)
