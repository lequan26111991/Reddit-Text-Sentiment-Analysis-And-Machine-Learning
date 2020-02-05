#install.packages("RedditExtractoR")
library(RedditExtractoR)
library(tidyverse)
library(tidytext)
library(wordcloud)


#gather data
r <- reddit_urls(subreddit = "worldnews", page_threshold = 5)
rc <- reddit_content(r$URL)
rc_rel <- rc[c("comment")]

#get tokens from comments
tokens <- rc_rel %>%
  unnest_tokens(output = word, input = comment)

#remove stopwords
sw = get_stopwords()
cleaned_tokens <- tokens %>%
  filter(!word %in% sw$word)

#remove some words based on regex
nums <- cleaned_tokens %>%
  filter(str_detect(word, "^[0-9]|http[s]?|.com$")) %>%
  select(word) %>% unique()

cleaned_tokens <- cleaned_tokens %>%
  filter(!word %in% nums$word)

#remove rare words
rare <- cleaned_tokens %>%
  count(word) %>%
  filter(n < 10) %>%
  select(word) %>%
  unique()

cleaned_tokens <- cleaned_tokens %>%
  filter(!word %in% rare$word)


#wordcloud
pal <- brewer.pal(8,"Dark2")
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = pal))

#sentiments
sent_reviews = cleaned_tokens %>%
  left_join(get_sentiments("nrc")) %>%
  rename(nrc = sentiment) %>%
  left_join(get_sentiments("bing")) %>%
  rename(bing = sentiment) %>%
  left_join(get_sentiments("afinn")) %>%
  rename(afinn = value)

#modifications for corona
#sent_reviews %>% filter(word == "corona")
sent_reviews <- sent_reviews %>%
  mutate(bing = ifelse(word == "corona", "negative", bing)) %>%
  mutate(nrc = ifelse(word == "corona", "negative", nrc))

#graph for bing top words
bing_word_counts <- sent_reviews %>%
  filter(!is.na(bing)) %>%
  count(word, bing, sort = TRUE)

bing_word_counts %>%
  filter(n > 100) %>%
  mutate(n = ifelse(bing == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = bing)) +  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

#graph for nrc top words
nrc_word_counts <- sent_reviews %>%
  filter(!is.na(nrc)) %>%
  count(word, nrc, sort = TRUE)

nrc_word_counts %>%
  filter(n > 50) %>%
  mutate(n = ifelse(nrc %in% c("negative", "fear", "anger", "sadness", "disgust"), -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = nrc)) +  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

#graph of the nrc category numbers
sent_reviews %>%
  filter(!is.na(nrc)) %>%
  ggplot(., aes(x = nrc)) +
  geom_bar()
