library(genius)
library(tidyverse)
library(corpus)
library(tidytext)
library(scales)
library(ggthemes)
library(DT)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(dplyr)

us <- read.csv('USvideos.csv')
category_count <- us %>% count(category = category_id)
df <- data.frame(category_count)
order <- order(df$n, decreasing = TRUE)
df$category[order]

df_24 <- filter(us, category_id == 24)
df_10 <- filter(us, category_id == 10)
df_26 <- filter(us, category_id == 26)
df_23 <- filter(us, category_id == 23)
df_22 <- filter(us, category_id == 22)

preprocess <- function(df){
  tags <- gsub("|", " ", df$tags, fixed=TRUE) %>% unique()
  channel_title <- as.character(unique(df$channel_title))
  diff <- setdiff(strsplit(tags, split = " "), strsplit(channel_title, split = " ")) %>% unique
  paste( unlist(diff), collapse=' ')
}

text_cleaning <- function(docs, v_rmwords){
  docs <- Corpus(VectorSource(docs))
  docs <- tm_map(docs, removePunctuation)
  # docs <- tm_map(docs, tolower)
  docs <- tm_map(docs,removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # docs <- tm_map(docs, stemDocument)
  docs <- tm_map(docs, removeWords, v_rmwords)
  Corpus(VectorSource(docs))
}

dtm <- function(docs){
  dtm <- DocumentTermMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(colSums(m),decreasing=TRUE)
  words <- names(v)
  data.frame(word=words, freq=v)
}

# category 24
docs24 <- preprocess(df_24)
v_rmwords <- c('ellen', 'show', 'video', 'trailer', 'the', 'movi')
docs <- text_cleaning(docs24, v_rmwords)
d24 <- dtm(docs)
wc24<- wordcloud(d24$word,d24$freq, type = 'text', colors=brewer.pal(8, "Dark2"), random.order = FALSE, rot.per=0.35, max.words = 200, scale=c(2.25,0.25))

# category 10
docs10 <- preprocess(df_10)
v_rmwords <- c('the', "music", 'video', 'record', 'trailer')
docs10 <- text_cleaning(docs10, v_rmwords)
d10 <- dtm(docs10)
wc10<- wordcloud(d10$word,d10$freq, type = 'text', lang = "english", colors=brewer.pal(8, "Dark2"), random.order = FALSE, rot.per=0.35, max.words = 200, scale=c(2.5,0.25))

# category 26
docs26 <- preprocess(df_26)
v_rmwords <- c("video", 'the', 'record', 'trailer', 'the')
docs26 <- text_cleaning(docs26, v_rmwords)
d26 <- dtm(docs26)
wc26<- wordcloud(d26$word,d26$freq, type = 'text', colors=brewer.pal(8, "Dark2"), random.order = FALSE, rot.per=0.3, max.words = 200, scale=c(3,0.25))

# category 23
docs23 <- preprocess(df_23)
v_rmwords <- c("show", 'the', 'video', 'trailer')
docs23 <- text_cleaning(docs23, v_rmwords)
d23 <- dtm(docs23)
wc23<- wordcloud(d23$word,d23$freq, type = 'text', lang = "english", colors=brewer.pal(8, "Dark2"), random.order = FALSE, rot.per=0.35, max.words = 200, scale=c(3,0.25))

# category 22
docs22 <- preprocess(df_22)
v_rmwords <- c("show", 'the', 'video', 'trailer')
docs22 <- text_cleaning(docs22, v_rmwords)
d22 <- dtm(docs22)
wc22<- wordcloud(d22$word,d22$freq, type = 'text', lang = "english", colors=brewer.pal(8, "Dark2"), random.order = FALSE, rot.per=0.35, max.words = 200, scale=c(2.9,0.25))
