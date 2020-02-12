
setwd("C:/Users/JNK4QBK/Desktop")

#install.packages("RedditExtractoR")
library(RedditExtractoR)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(stringr)

r <- reddit_urls(subreddit = "worldnews", page_threshold = 5)
rc <- reddit_content(r$URL)

# Aggregate Corona Virus
df_corona <- rc %>% filter(str_detect(title,"corona") | 
                             str_detect(title,"Corona") |
                             str_detect(title,"CORONA") |
                             str_detect(title,"2019-nCOV")) %>%
  select(comm_date) %>%
  group_by(comm_date) %>% 
  count(comm_date)

#change the file Name of Corona
c_name <- c("Date","Count_Corona")
colnames(df_corona) <- c_name

# Aggregate World
df_world <- rc %>% 
  select(comm_date) %>%
  group_by(comm_date) %>%
  count(comm_date)

#change the file Name of World
w_name <- c("Date","Count_World")
colnames(df_world) <- w_name

df_export <- inner_join(df_world,df_corona, by ="Date")

if (file.exists("corona_over_time.csv") == TRUE){
write.table(df_export,"corona_over_time.csv",
            append = TRUE, col.names = FALSE, sep =",", row.names=FALSE)
} else {
write.table(df_export,"corona_over_time.csv",
            append = FALSE, col.names = TRUE, sep =",", row.names=FALSE)
}

#Set schedule every day feeding data

install.packages("taskscheduleR")
library(taskscheduleR)
myscript <- system.file("reddit_corona.R")

taskscheduler_create(taskname = "CORONA_UPDATE", 
                     rscript = "C:/Users/JNK4QBK/Desktop/reddit_corona.R",schedule = "DAILY", 
                     starttime = "23:50", startdate = format(Sys.Date()+1, "%d/%m/%Y"))