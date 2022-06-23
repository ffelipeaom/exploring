#phantom of the opera vs pride and prejudice
install.packages("ggwordcloud")
install.packages("patchwork")

library("dplyr")
library("ggplot2")
library("ggwordcloud")
library("gutenbergr")
library("patchwork")
library("SnowballC")
library("stringr")
library("tidytext")
library("wordcloud")

gberg <- gutenberg_metadata

#baixando as obras e isolando em um objeto
ppp <- gutenberg_download(c(175, 1342))

nums <- ppp %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 
ppp <- ppp %>% anti_join(nums, by = "text")

#unnest token: isolando cada token (=palavra)
ppp_token <- ppp %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)

#calcula frequencia do termo, frequencia inversa e a relacao entre eles
ppp_tfidf <- ppp_token %>% bind_tf_idf(word, gutenberg_id, n)

#separa os df
phantom_tfidf <- ppp_tfidf %>% filter(gutenberg_id == 175)
pride_tfidf <- ppp_tfidf %>% filter(gutenberg_id == 1342)

phantom_unique <- phantom_tfidf %>% anti_join(pride_tfidf, by = "word")
pride_unique <- pride_tfidf %>% anti_join(phantom_tfidf, by = "word")

#semijoin pra manter as tokens em comum: remove nomes, mantem stopwords
ppp_common <- ppp_tfidf %>% semi_join(pride_tfidf, by = "word") #%>% semi_join(phantom_tfidf, by = "word")

#cleaning: remove stopwords, no stemming
ppp_common_clean <- ppp_common %>% anti_join(stop_words)# %>% mutate(stem = wordStem(word))
#ppp2 <- ppp_common_clean %>% count(stem, sort = TRUE)

phantom_clean <- ppp_common_clean %>% filter(gutenberg_id == 175) %>% anti_join(phantom_unique) %>% select(c(2:4)) 
pride_clean <- ppp_common_clean %>% filter(gutenberg_id == 1342) %>% anti_join(pride_unique) %>% select(c(2:4))

#separa as top50 tokens de cada df, baseado no tf-idf e ignorando empates para
phantom_plot <- phantom_clean %>% slice_max(tf, n = 50) %>% 
  ggplot(aes(
    label = word,
    size = tf*100,
    color = n)) +
  geom_text_wordcloud(rm_outside = TRUE, eccentricity = 1) +
  scale_radius(range = c(0, 20), limits = c(0, NA)) +
  theme_minimal() +
   scale_color_gradient(low = "darkred", high = "red")
pride_plot <- pride_clean %>% slice_max(tf, n = 50) %>% 
  ggplot(aes(
    label = word,
    size = tf*100,
    color = n)) + #factor(sample.int(1, 51, replace = TRUE))
  geom_text_wordcloud(rm_outside = TRUE, eccentricity = 1) +
# sscale_size_area(max_size = 10) +
  scale_radius(range = c(0, 20), limits = c(0, NA)) +
  theme_minimal() #+
# scale_color_gradient(low = "darkred", high = "red")

phantom_plot + pride_plot

#cria df com as tokens mais relevantemente frequentes de cada obra
#ppp_plot <- ppp_tfidf %>% 
#  group_by(gutenberg_id) %>% #agrupa por id (ao invés de frequencia?)
#  slice_max(tf_idf, n = 15) %>% #separa as top15 de cada grupo, baseado no tfidf
#  ungroup() %>% 
#  mutate(word = reorder(word, tf_idf)) #coloca em ordem decrescente de tfidf

#e plota
#ppp_plot %>% ggplot(aes(tf_idf, word, fill = gutenberg_id)) +
#  geom_col(show.legend = FALSE) +
#  labs(x = "tf-idf", y = NULL) +
#  facet_wrap(~gutenberg_id, ncol = 2, scales = "free")

# define a paleta de cores
#phantom_pal <- brewer.pal(8,"Dark2")
#pride_pal <- brewer.pal(8, "Paired")

#word cloud
#ppp_plot %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=palette))
#phantom_plot %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors = phantom_pal))
#pride_plot %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors = pride_pal))



# TODO: stem words, remove names, plot both as word cloud side by side








