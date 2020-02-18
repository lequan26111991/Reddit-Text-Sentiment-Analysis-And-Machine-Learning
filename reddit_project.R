#install.packages("RedditExtractoR")
library(RedditExtractoR)
library(tidyverse)
library(wordcloud)
library(tidytext)
library(recipes)
library(skimr)
library(h2o)

#gather data
r <- reddit_urls(subreddit = "Coronavirus", page_threshold = 20)
rc <- reddit_content(r$URL)
rc_rel <- rc[c("comment", "comment_score", "post_score")]
rc_rel <- mutate(rc_rel, id = rownames(rc_rel))

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
  filter(n > 250) %>%
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
  filter(n > 250) %>%
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


#machine learning
#################

#id as integer
sent_reviews <- sent_reviews %>% mutate(id = as.integer(id))

#base df
df0 <- sent_reviews %>% select("id", "comment_score", "post_score") %>% unique()

#these will be the columns basically
nrcs <- sent_reviews %>% select("nrc") %>% unique() %>% na.omit() %>% as.list()

for (nrcx in nrcs$nrc) {
  df1 <- sent_reviews %>%
    select("id", "nrc") %>%
    filter(nrc == nrcx) %>%
    group_by(id) %>%
    summarise(num = n()) %>%
    arrange(id)
  
  colnames(df1)[2] <- paste0("nrc_", nrcx)
  
  df0 <- full_join(df0, df1, by = "id")
}

x_train_tbl <- df0 %>% select(-c(comment_score, id))
y_train_tbl <- df0 %>% select(comment_score)

x_train_tbl_skim = partition(skim(x_train_tbl))

rec_obj <- recipe(~ ., data = x_train_tbl) %>%
  step_meanimpute(all_numeric()) %>% # missing values in numeric columns
  prep(training = x_train_tbl)

x_train_processed_tbl <- bake(rec_obj, x_train_tbl)

#h2o
h2o.init(nthreads = -1)
h2o.clusterInfo()
h2o.removeAll()
data_h2o <- as.h2o(
  bind_cols(y_train_tbl, x_train_processed_tbl),
  destination_frame= "train.hex"
)

splits <- h2o.splitFrame(data = data_h2o,
                         ratios = c(0.7, 0.15), # 70/15/15 split
                         seed = 1234)
train_h2o <- splits[[1]]
valid_h2o <- splits[[2]]
test_h2o  <- splits[[3]]

y <- "comment_score"
x <- setdiff(names(train_h2o), y)

hyper_prms <- list(
  ntrees = c(45, 50, 55),
  learn_rate = c(0.1, 0.2),
  max_depth = c(20, 25)
)

grid_gbm <- h2o.grid(
  grid_id="grid_gbm",
  algorithm = "gbm",
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  x = x,
  y = y,
  sample_rate = 0.7,
  col_sample_rate = 0.7,
  stopping_rounds = 2,
  stopping_metric = "RMSE",
  stopping_tolerance = 0.0001,
  score_each_iteration = T,
  model_id = "gbm_covType3",
  balance_classes = T,
  seed = 5000,
  hyper_params = hyper_prms
)

grid <- h2o.getGrid("grid_gbm", sort_by = "rmse", decreasing = FALSE)
dl_grid_best_model <- h2o.getModel(grid@summary_table$model_ids[1])
