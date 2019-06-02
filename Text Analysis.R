#Installing the Libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("janaustenr")
install.packages("tidytext")
install.packages("widyr")
install.packages("devtools")
install.packages("tidyr")
install.packages("igraph")
install.packages("ggraph")
install.packages("chron")
install.packages("readr")
install.packages("twitteR")
install.packages("tm")
install.packages("NLP")
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("topicmodels")
install.packages("stringi")
install.packages("stringr")

#Loading the libraries
library(ggplot2)
library(igraph)
library(ggraph)
library(tidyr)
library(widyr)
library(devtools)
library(dplyr)
library(janeaustenr)
library(tidytext)
library(chron)
library("NLP", lib.loc="~/R/win-library/3.3")
library("twitteR", lib.loc="~/R/win-library/3.3")
library("syuzhet")
library("tm", lib.loc="~/R/win-library/3.3")
library("SnowballC")
library("stringi")
library("topicmodels")
library("tm")
library("wordcloud")
library("readr")
library("stringr")




#AntiCorruptionDay Tweets
anticorruption_tweets <- read.csv("banptmnow 12332.csv")

anticorruption <- as.list(anticorruption_tweets)

class(anticorruption_tweets$text)

anticorruption_tweets_clean <- anticorruption_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)

nrow(anticorruption_tweets_clean)
## [1] 133650

# remove stop words from your list of words
anticorruption_tweet_words <- anticorruption_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(anticorruption_tweet_words)

anticorruption_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#####PLOTTING#######

# remove punctuation, convert to lowercase, add id for each tweet!
anticorruption_tweets_paired_words <- anticorruption_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

anticorruption_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#####

anticorruption_tweets_separated_words <- anticorruption_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

anticorruption_tweets_filtered <- anticorruption_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
anticorruption_words_counts <- anticorruption_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(anticorruption_words_counts)

# plot anticorruption change word network
anticorruption_words_counts %>%
  filter(n >= 70) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - #AntiCorruptionDay",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#Top screenNames
anticorruptiontable <- sort(table(anticorruption_tweets$screenName), decreasing = TRUE)
anticorruptiontable[1:30]

#Dividing the timestamp column into date and time
anticorruptiontime <- substr(anticorruption_tweets$created, 12, 19)
anticorruptiontime <- times(anticorruptiontime)

anticorruptiondate <- substr(anticorruption_tweets$created, 1, 10)
anticorruptiondate <- format(as.Date(anticorruptiondate), "%m/%d/%Y")
anticorruptiondate <- dates(anticorruptiondate)

anticorruptiondatetime <- chron(dates. = anticorruptiondate, times. = anticorruptiontime)

#Adding extra columns to the original dataframe 
anticorruption_tweets$date <- anticorruptiondatetime

#Timestamp Visualizations
ggplot(anticorruption_tweets, aes(x= as.POSIXct(anticorruption_tweets$date))) + geom_density(fill = "light blue") + scale_x_datetime() + xlab("Date and Time") + ggtitle("#AntiCorruptionDay Tweets") + theme_classic()

ggplot(anticorruption_tweets, aes(x= as.POSIXct(anticorruption_tweets$date))) + geom_density(aes(fill = anticorruption_tweets$isRetweet), alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#AntiCorruptionDay Tweets") + theme_classic() + labs(fill = "is Retweet")
ggplot(anticorruption_tweets, aes(x= as.POSIXct(anticorruption_tweets$date))) + geom_density( alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#AntiCorruptionDay Tweets") + theme_classic()




#StandwithFIA Tweets
fiastand_tweets <- read.csv("FIA Stand 11324.csv")

fiastand <- as.list(fiastand_tweets)

class(fiastand_tweets$text)

fiastand_tweets_clean <- fiastand_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)

nrow(fiastand_tweets_clean)
## [1] 133650

# remove stop words from your list of words
fiastand_tweet_words <- fiastand_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(fiastand_tweet_words)

fiastand_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#####PLOTTING#######

# remove punctuation, convert to lowercase, add id for each tweet!
fiastand_tweets_paired_words <- fiastand_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

fiastand_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#####

fiastand_tweets_separated_words <- fiastand_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

fiastand_tweets_filtered <- fiastand_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
fiastand_words_counts <- fiastand_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(fiastand_words_counts)

# plot fiastand change word network
fiastand_words_counts %>%
  filter(n >= 70) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - #WeStandWithFIA",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#Top screenNames
fiastandtable <- sort(table(fiastand_tweets$screenName), decreasing = TRUE)
fiastandtable[1:30]

#Dividing the timestamp column into date and time
fiastandtime <- substr(fiastand_tweets$created, 12, 19)
fiastandtime <- times(fiastandtime)

fiastanddate <- substr(fiastand_tweets$created, 1, 10)
fiastanddate <- format(as.Date(fiastanddate), "%m/%d/%Y")
fiastanddate <- dates(fiastanddate)

fiastanddatetime <- chron(dates. = fiastanddate, times. = fiastandtime)

#Adding extra columns to the original dataframe 
fiastand_tweets$date <- fiastanddatetime

#Timestamp Visualizations
ggplot(fiastand_tweets, aes(x= as.POSIXct(fiastand_tweets$date))) + geom_density(fill = "light blue") + scale_x_datetime() + xlab("Date and Time") + ggtitle("#WeStandWithFIA Tweets") + theme_classic()

ggplot(fiastand_tweets, aes(x= as.POSIXct(fiastand_tweets$date))) + geom_density(aes(fill = fiastand_tweets$isRetweet), alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#WeStandWithFIA Tweets") + theme_classic() + labs(fill = "is Retweet")
ggplot(fiastand_tweets, aes(x= as.POSIXct(fiastand_tweets$date))) + geom_density( alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#WeStandWithFIA Tweets") + theme_classic()




#ShameonFIA Tweets
fiashame_tweets <- read.csv("FIA Shame 9509.csv")

fiashame <- as.list(fiashame_tweets)

class(fiashame_tweets$text)

fiashame_tweets_clean <- fiashame_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)

nrow(fiashame_tweets_clean)
## [1] 133650

# remove stop words from your list of words
fiashame_tweet_words <- fiashame_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(fiashame_tweet_words)

fiashame_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#####PLOTTING#######

# remove punctuation, convert to lowercase, add id for each tweet!
fiashame_tweets_paired_words <- fiashame_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

fiashame_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#####

fiashame_tweets_separated_words <- fiashame_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

fiashame_tweets_filtered <- fiashame_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
fiashame_words_counts <- fiashame_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(fiashame_words_counts)

# plot fiashame change word network
fiashame_words_counts %>%
  filter(n >= 70) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - #ShameOnFIA",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#Top screenNames
fiashametable <- sort(table(fiashame_tweets$screenName), decreasing = TRUE)
fiashametable[1:30]

#Dividing the timestamp column into date and time
fiashametime <- substr(fiashame_tweets$created, 12, 19)
fiashametime <- times(fiashametime)

fiashamedate <- substr(fiashame_tweets$created, 1, 10)
fiashamedate <- format(as.Date(fiashamedate), "%m/%d/%Y")
fiashamedate <- dates(fiashamedate)

fiashamedatetime <- chron(dates. = fiashamedate, times. = fiashametime)

#Adding extra columns to the original dataframe 
fiashame_tweets$date <- fiashamedatetime

#Timestamp Visualizations
ggplot(fiashame_tweets, aes(x= as.POSIXct(fiashame_tweets$date))) + geom_density(fill = "light blue") + scale_x_datetime() + xlab("Date and Time") + ggtitle("#ShameOnFIA Tweets") + theme_classic()

ggplot(fiashame_tweets, aes(x= as.POSIXct(fiashame_tweets$date))) + geom_density(aes(fill = fiashame_tweets$isRetweet), alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#ShameOnFIA Tweets") + theme_classic() + labs(fill = "is Retweet")
ggplot(fiashame_tweets, aes(x= as.POSIXct(fiashame_tweets$date))) + geom_density( alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#ShameOnFIA Tweets") + theme_classic()




#Pashteen Tweets
pashteen_tweets <- read.csv("pashteen 12357.csv")

pashteen <- as.list(pashteen_tweets)

class(pashteen_tweets$text)

pashteen_tweets_clean <- pashteen_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)

nrow(pashteen_tweets_clean)
## [1] 133650

# remove stop words from your list of words
pashteen_tweet_words <- pashteen_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(pashteen_tweet_words)

pashteen_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#####PLOTTING#######

# remove punctuation, convert to lowercase, add id for each tweet!
pashteen_tweets_paired_words <- pashteen_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

pashteen_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#####

pashteen_tweets_separated_words <- pashteen_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

pashteen_tweets_filtered <- pashteen_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
pashteen_words_counts <- pashteen_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(pashteen_words_counts)

# plot pashteen change word network
pashteen_words_counts %>%
  filter(n >= 80) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - #Pashteen",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#Top screenNames
pashteentable <- sort(table(pashteen_tweets$screenName), decreasing = TRUE)
pashteentable[1:30]

#Dividing the timestamp column into date and time
pashteentime <- substr(pashteen_tweets$created, 12, 19)
pashteentime <- times(pashteentime)

pashteendate <- substr(pashteen_tweets$created, 1, 10)
pashteendate <- format(as.Date(pashteendate), "%m/%d/%Y")
pashteendate <- dates(pashteendate)

pashteendatetime <- chron(dates. = pashteendate, times. = pashteentime)

#Adding extra columns to the original dataframe 
pashteen_tweets$date <- pashteendatetime

#Timestamp Visualizations
ggplot(pashteen_tweets, aes(x= as.POSIXct(pashteen_tweets$date))) + geom_density(fill = "light blue") + scale_x_datetime() + xlab("Date and Time") + ggtitle("Pashteen Tweets") + theme_classic()

ggplot(pashteen_tweets, aes(x= as.POSIXct(pashteen_tweets$date))) + geom_density(aes(fill = pashteen_tweets$isRetweet), alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("Pashteen Tweets") + theme_classic() + labs(fill = "is Retweet")
ggplot(pashteen_tweets, aes(x= as.POSIXct(pashteen_tweets$date))) + geom_density( alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("Pashteen Tweets") + theme_classic()




#OurPeacefulBalochistan Tweets
balochistan_tweets <- read.csv("13787 #OurPeacefulBalochistan.csv")

balochistan <- as.list(balochistan_tweets)

class(balochistan_tweets$text)

balochistan_tweets_clean <- balochistan_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)

nrow(balochistan_tweets_clean)
## [1] 133650

# remove stop words from your list of words
balochistan_tweet_words <- balochistan_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(balochistan_tweet_words)

balochistan_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#####PLOTTING#######

# remove punctuation, convert to lowercase, add id for each tweet!
balochistan_tweets_paired_words <- balochistan_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

balochistan_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#####

balochistan_tweets_separated_words <- balochistan_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

balochistan_tweets_filtered <- balochistan_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
balochistan_words_counts <- balochistan_tweets_filtered %>%
  count(word1, word2, sort = TRUE) 

head(balochistan_words_counts)

# plot balochistan change word network
balochistan_words_counts %>%
  filter(n >= 60) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - #OurPeacefulBalochistan",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#Top screenNames
balochistantable <- sort(table(balochistan_tweets$screenName), decreasing = TRUE)
balochistantable[1:30]

#Dividing the timestamp column into date and time
balochistantime <- substr(balochistan_tweets$created, 12, 19)
balochistantime <- times(balochistantime)

balochistandate <- substr(balochistan_tweets$created, 1, 10)
balochistandate <- format(as.Date(balochistandate), "%m/%d/%Y")
balochistandate <- dates(balochistandate)

balochistandatetime <- chron(dates. = balochistandate, times. = balochistantime)

#Adding extra columns to the original dataframe 
balochistan_tweets$date <- balochistandatetime

#Timestamp Visualizations
ggplot(balochistan_tweets, aes(x= as.POSIXct(balochistan_tweets$date))) + geom_density(fill = "light blue") + scale_x_datetime() + xlab("Date and Time") + ggtitle("#OurPeacefulBalochistan Tweets") + theme_classic()

ggplot(balochistan_tweets, aes(x= as.POSIXct(balochistan_tweets$date))) + geom_density(aes(fill = balochistan_tweets$isRetweet), alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#OurPeacefulBalochistan Tweets") + theme_classic() + labs(fill = "is Retweet")
ggplot(balochistan_tweets, aes(x= as.POSIXct(balochistan_tweets$date))) + geom_density( alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("#OurPeacefulBalochistan Tweets") + theme_classic()




#ImranKhan Tweets
imran_tweets <- read.csv("Imran Khan.csv")

imran <- as.list(imran_tweets)

class(imran_tweets$text)

imran_tweets_clean <- imran_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)

nrow(imran_tweets_clean)
## [1] 133650

# remove stop words from your list of words
imran_tweet_words <- imran_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(imran_tweet_words)

imran_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

#####PLOTTING#######

# remove punctuation, convert to lowercase, add id for each tweet!
imran_tweets_paired_words <- imran_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

imran_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#####

imran_tweets_separated_words <- imran_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

imran_tweets_filtered <- imran_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
imran_words_counts <- imran_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(imran_words_counts)

# plot imran change word network
imran_words_counts %>%
  filter(n >= 80) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - #ImranKhan",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

#Top screenNames
imrantable <- sort(table(imran_tweets$screenName), decreasing = TRUE)
imrantable[1:30]

#Dividing the timestamp column into date and time
imrantime <- substr(imran_tweets$created, 12, 16)
imrantime <- paste(imrantime, ":00", sep = "")
imrantime <- times(imrantime)

imrandate <- substr(imran_tweets$created, 1, 10)
imrandate <- dates(imrandate)

imrandatetime <- chron(dates. = imrandate, times. = imrantime)

#Adding extra columns to the original dataframe 
imran_tweets$date <- imrandatetime

#Timestamp Visualizations
ggplot(imran_tweets, aes(x= as.POSIXct(imran_tweets$date))) + geom_density(fill = "light blue") + scale_x_datetime() + xlab("Date and Time") + ggtitle("Imran Khan Tweets") + theme_classic()

ggplot(imran_tweets, aes(x= as.POSIXct(imran_tweets$date))) + geom_density(aes(fill = imran_tweets$isRetweet), alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("Imran Khan Tweets") + theme_classic() + labs(fill = "is Retweet")
ggplot(imran_tweets, aes(x= as.POSIXct(imran_tweets$date))) + geom_density( alpha = 0.5) + scale_x_datetime() + xlab("Date and Time") + ggtitle("Imran Khan Tweets") + theme_classic()





#Sentiment Analysis
balochistan_tlist <- as.list(balochistan_tweets)
imran_tlist <- as.list(imran_tweets)
anticorruption_tlist <- as.list(anticorruption_tweets)
fiashame_tlist <- as.list(fiashame_tweets)
fiastand_tlist <- as.list(fiastand_tweets)
pashteen_tlist <- as.list(pashteen_tweets)

#keeping only the text
balochistan_text<- balochistan_tweets$text
imran_text<- imran_tweets$text
anticorruption_text<- anticorruption_tweets$text
fiashame_text<- fiashame_tweets$text
fiastand_text<- fiastand_tweets$text
pashteen_text<- pashteen_tweets$text

#convert all text to lower case
balochistan_text<- tolower(balochistan_text)
imran_text<- tolower(imran_text)
anticorruption_text<- tolower(anticorruption_text)
fiashame_text<- tolower(fiashame_text)
fiastand_text<- tolower(fiastand_text)
pashteen_text<- tolower(pashteen_text)

# Replace @UserName
balochistan_text <- gsub("@\\w+", "", balochistan_text)
imran_text <- gsub("@\\w+", "", imran_text)
anticorruption_text <- gsub("@\\w+", "", anticorruption_text)
fiashame_text <- gsub("@\\w+", "", fiashame_text)
fiastand_text <- gsub("@\\w+", "", fiastand_text)
pashteen_text <- gsub("@\\w+", "", pashteen_text)

# Replacing all non-alphanumeric characters with space
balochistan_text <- str_replace_all(balochistan_text, "[^[:alnum:]]", " ")
imran_text <- str_replace_all(imran_text, "[^[:alnum:]]", " ")
anticorruption_text <- str_replace_all(anticorruption_text, "[^[:alnum:]]", " ")
fiashame_text <- str_replace_all(fiashame_text, "[^[:alnum:]]", " ")
fiastand_text <- str_replace_all(fiastand_text, "[^[:alnum:]]", " ")
pashteen_text <- str_replace_all(pashteen_text, "[^[:alnum:]]", " ")

# Replace blank space (“rt”)
balochistan_text <- gsub("rt", "", balochistan_text)
imran_text <- gsub("rt", "", imran_text)
anticorruption_text <- gsub("rt", "", anticorruption_text)
fiashame_text <- gsub("rt", "", fiashame_text)
fiastand_text <- gsub("rt", "", fiastand_text)
pashteen_text <- gsub("rt", "", pashteen_text)

# Remove punctuation
balochistan_text <- gsub("[[:punct:]]", "", balochistan_text)
imran_text <- gsub("[[:punct:]]", "", imran_text)
anticorruption_text <- gsub("[[:punct:]]", "", anticorruption_text)
fiashame_text <- gsub("[[:punct:]]", "", fiashame_text)
fiastand_text <- gsub("[[:punct:]]", "", fiastand_text)
pashteen_text <- gsub("[[:punct:]]", "", pashteen_text)

# Remove links
balochistan_text <- gsub("http\\w+", "", balochistan_text)
imran_text <- gsub("http\\w+", "", imran_text)
anticorruption_text <- gsub("http\\w+", "", anticorruption_text)
fiashame_text <- gsub("http\\w+", "", fiashame_text)
fiastand_text <- gsub("http\\w+", "", fiastand_text)
pashteen_text <- gsub("http\\w+", "", pashteen_text)

# Remove tabs
balochistan_text <- gsub("[ |\t]{2,}", "", balochistan_text)
imran_text <- gsub("[ |\t]{2,}", "", imran_text)
anticorruption_text <- gsub("[ |\t]{2,}", "", anticorruption_text)
fiashame_text <- gsub("[ |\t]{2,}", "", fiashame_text)
fiastand_text <- gsub("[ |\t]{2,}", "", fiastand_text)
pashteen_text <- gsub("[ |\t]{2,}", "", pashteen_text)


# Remove blank spaces at the beginning
balochistan_text <- gsub("^ ", "", balochistan_text)
imran_text <- gsub("^ ", "", imran_text)
anticorruption_text <- gsub("^ ", "", anticorruption_text)
fiashame_text <- gsub("^ ", "", fiashame_text)
fiastand_text <- gsub("^ ", "", fiastand_text)
pashteen_text <- gsub("^ ", "", pashteen_text)

# Remove blank spaces at the end
balochistan_text <- gsub(" $", "", balochistan_text)
imran_text <- gsub(" $", "", imran_text)
anticorruption_text <- gsub(" $", "", anticorruption_text)
fiashame_text <- gsub(" $", "", fiashame_text)
fiastand_text <- gsub(" $", "", fiastand_text)
pashteen_text <- gsub(" $", "", pashteen_text)

#create corpus
balochistan_tweets.text.corpus <- Corpus(VectorSource(balochistan_text))
imran_tweets.text.corpus <- Corpus(VectorSource(imran_text))
anticorruption_tweets.text.corpus <- Corpus(VectorSource(anticorruption_text))
fiashame_tweets.text.corpus <- Corpus(VectorSource(fiashame_text))
fiastand_tweets.text.corpus <- Corpus(VectorSource(fiastand_text))
pashteen_tweets.text.corpus <- Corpus(VectorSource(pashteen_text))


#clean up by removing stop words
balochistan_tweets.text.corpus <- tm_map(balochistan_tweets.text.corpus, function(x)removeWords(x,stopwords()))
imran_tweets.text.corpus <- tm_map(imran_tweets.text.corpus, function(x)removeWords(x,stopwords()))
anticorruption_tweets.text.corpus <- tm_map(anticorruption_tweets.text.corpus, function(x)removeWords(x,stopwords()))
fiashame_tweets.text.corpus <- tm_map(fiashame_tweets.text.corpus, function(x)removeWords(x,stopwords()))
fiastand_tweets.text.corpus <- tm_map(fiastand_tweets.text.corpus, function(x)removeWords(x,stopwords()))
pashteen_tweets.text.corpus <- tm_map(pashteen_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#WordCloud
wordcloud(balochistan_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(imran_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(anticorruption_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(fiashame_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(fiastand_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(pashteen_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#Sentiment Analysis
#getting emotions using in-built function
mysentiment_balochistan<-get_nrc_sentiment((balochistan_text))
mysentiment_imran<-get_nrc_sentiment((imran_text))
mysentiment_anticorruption<-get_nrc_sentiment((anticorruption_text))
mysentiment_fiashame<-get_nrc_sentiment((fiashame_text))
mysentiment_fiastand<-get_nrc_sentiment((fiastand_text))
mysentiment_pashteen<-get_nrc_sentiment((pashteen_text))

#calculationg total score for each sentiment
Sentimentscores_balochistan<-data.frame(colSums(mysentiment_balochistan[,]))
Sentimentscores_imran<-data.frame(colSums(mysentiment_imran[,]))
Sentimentscores_anticorruption<-data.frame(colSums(mysentiment_anticorruption[,]))
Sentimentscores_fiashame<-data.frame(colSums(mysentiment_fiashame[,]))
Sentimentscores_fiastand<-data.frame(colSums(mysentiment_fiastand[,]))
Sentimentscores_pashteen<-data.frame(colSums(mysentiment_pashteen[,]))

names(Sentimentscores_balochistan)<-"Score"
Sentimentscores_balochistan<-cbind("sentiment"=rownames(Sentimentscores_balochistan),Sentimentscores_balochistan)
rownames(Sentimentscores_balochistan)<-NULL


names(Sentimentscores_imran)<-"Score"
Sentimentscores_imran<-cbind("sentiment"=rownames(Sentimentscores_imran),Sentimentscores_imran)
rownames(Sentimentscores_imran)<-NULL

names(Sentimentscores_anticorruption)<-"Score"
Sentimentscores_anticorruption<-cbind("sentiment"=rownames(Sentimentscores_anticorruption),Sentimentscores_anticorruption)
rownames(Sentimentscores_anticorruption)<-NULL

names(Sentimentscores_fiashame)<-"Score"
Sentimentscores_fiashame<-cbind("sentiment"=rownames(Sentimentscores_fiashame),Sentimentscores_fiashame)
rownames(Sentimentscores_fiashame)<-NULL

names(Sentimentscores_fiastand)<-"Score"
Sentimentscores_fiastand<-cbind("sentiment"=rownames(Sentimentscores_fiastand),Sentimentscores_fiastand)
rownames(Sentimentscores_fiastand)<-NULL

names(Sentimentscores_pashteen)<-"Score"
Sentimentscores_pashteen<-cbind("sentiment"=rownames(Sentimentscores_pashteen),Sentimentscores_pashteen)
rownames(Sentimentscores_pashteen)<-NULL


#plotting the sentiments with scores
ggplot(data=Sentimentscores_balochistan,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets with #OurPeacefulBalochistan hashtag")


ggplot(data=Sentimentscores_imran,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets containing the keyword Imran Khan")


ggplot(data=Sentimentscores_anticorruption,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets containing #AntiCorruptionDay Hashtag")

ggplot(data=Sentimentscores_fiashame,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets containing #ShameonFIA hashtag")

ggplot(data=Sentimentscores_fiastand,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets containing #WeStandwithFIA hashtag")

ggplot(data=Sentimentscores_pashteen,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets containing the keyword Pashteen")
