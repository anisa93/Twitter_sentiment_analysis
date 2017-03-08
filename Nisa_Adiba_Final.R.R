#installing packages
install.packages("twitteR")
install.packages("ROAuth")
install.packages("bitops")
install.packages("rjson")
install.packages("RCurl")
install.packages("NLP")
install.packages("tm")
install.packages("SnowballC")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("stringi")
install.packages("tm.plugin.webmining")
install.packages("ggplot2")
install.packages("Rstem")
library(twitteR)
library(ROAuth)
library(bitops)
library(rjson)
library(RCurl)
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(stringi)
library(tm.plugin.webmining)
library(ggplot2)
library(Rstem)

#installing sentiment package since it is not available using R follow below steps-
#Step 1- download sentiment_0.2.tar.gz package online and place it in C drive then run below command.
install.packages("C:/sentiment_0.2.tar.gz", repos = NULL, type="source")
library(sentiment)

setwd("C:/Users/Adiba/Documents/WM_Proj")

#connect to the twitter App

t.api.key <- "CL3IUNz9cbB8Jf0ZyZpte0HDl"
t.api.secret <- "UANK3ox6ajLDuUj1ZsiBIVducj0pHzU9b0nY7X0kgmU9ZI4Eur"

setup_twitter_oauth(t.api.key, t.api.secret, access_token="3372528798-1Uui92saNOiqvQ7sn73R1wY9v4hPhabbGgTh2VO", access_secret="LHr6DvlN3VRcvNWE6vEumoBtDZzoOq5hBh4RHKY5eQeMe")   #Select option 1.

###### Stock Gainers on 12/20/2016 #######################

tweets_kmx <- searchTwitter(c("$KMX"),n = 30)
tweets_trip <- searchTwitter(c("$TRIP"),n = 100)
tweets_fcx <- searchTwitter(c("$FCX"),n = 100)
tweets_gainer <- c(tweets_kmx,tweets_trip,tweets_fcx)
head(tweets_gainer)

###### Stock Losers on 12/20/2016 ########################

tweets_ayi <- searchTwitter(c("$AYI"),n = 100)
tweets_stz <- searchTwitter(c("$STZ"),n = 100)
tweets_px <- searchTwitter(c("$PX"),n = 100)
tweets_losers <- c(tweets_ayi,tweets_stz,tweets_px)
head(tweets_losers)



# Corpus function


get_Corpus <- function(x){
  tweets_text <- lapply(x, function(t) { t$getText()})
  temp <- VectorSource(tweets_text)
  data_corpus <- Corpus(temp)
  return(data_corpus)
}

data_corpus1 <- get_Corpus(tweets_gainer)
data_corpus2 <- get_Corpus(tweets_losers)


inspect(data_corpus1[1:2])

inspect(data_corpus2[1:2])

##########################################################################################################
writeCorpus(data_corpus1)
writeCorpus(data_corpus2)


#function for preprocessing 


#preprocessing
#PlainTextDocument
data_corpus1 <- tm_map(data_corpus1, PlainTextDocument, lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, PlainTextDocument, lazy = TRUE)

#RemovePunctuation
data_corpus1 <- tm_map(data_corpus1, content_transformer(removePunctuation), lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, content_transformer(removePunctuation), lazy = TRUE)

#RemoveURL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
data_corpus1 <- tm_map(data_corpus1, content_transformer(removeURL), lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, content_transformer(removeURL), lazy = TRUE)

#RemoveNumber
data_corpus1 <- tm_map(data_corpus1, removeNumbers, lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, removeNumbers, lazy = TRUE)

#RemoveNumberPunctuationWhitespaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
data_corpus1 <- tm_map(data_corpus1, content_transformer(removeNumPunct), lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, content_transformer(removeNumPunct), lazy = TRUE)

#extrawhitespace
data_corpus1 <- tm_map(data_corpus1, stripWhitespace, lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, stripWhitespace, lazy = TRUE)

#RemoveWords
data_corpus1 <- tm_map(data_corpus1,removeWords, stopwords("en"))
data_corpus2 <- tm_map(data_corpus2,removeWords, stopwords("en"))

#To Lower
data_corpus1 <- tm_map(data_corpus1, content_transformer(tolower), lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, content_transformer(tolower), lazy = TRUE)

#PlainTextDocument
data_corpus1 <- tm_map(data_corpus1, PlainTextDocument, lazy = TRUE)
data_corpus2 <- tm_map(data_corpus2, PlainTextDocument, lazy = TRUE)



# remove NAs in data.corpus
data_corpus1 = data_corpus1[!is.na(data_corpus1)]
names(data_corpus1) = NULL
data_corpus2 = data_corpus2[!is.na(data_corpus2)]
names(data_corpus2) = NULL



writeCorpus(data_corpus1)
writeCorpus(data_corpus2)

writeLines(as.character(data_corpus1), con = "datacorpus1.txt")
writeLines(as.character(data_corpus2), con = "datacorpus2.txt")

#

for (i in 6:16){
  print(data_corpus1[[i]]$content)
}

for (i in 6:16){
  print(data_corpus2[[i]]$content)
}


#Term document matrix 

tdm_1 <- TermDocumentMatrix(data_corpus1)
tdm_2 <- TermDocumentMatrix(data_corpus2)
tdm_1
tdm_2

inspect(tdm_1[10:30,80:100])
inspect(tdm_2[150:170,80:100])

save(list = (c("tdm_1","tdm_2")),file = "term.document.matrix")

# Frequent terms

findFreqTerms(tdm_1,lowfreq = 10)

findFreqTerms(tdm_2,lowfreq = 10)

#Most frequent words 

Freq_gainers <- rowSums(as.matrix(tdm_1))
Freq_gainers <- sort(Freq_gainers,decreasing = TRUE)
Freq_losers <- rowSums(as.matrix(tdm_2))
Freq_losers <- sort(Freq_losers,decreasing = TRUE)
cbind(Freq_gainers[1:10])
cbind(Freq_losers[1:10])


# Wordcloud 
set.seed(124)

# Gainers wordcloud

wordcloud(words = names(Freq_gainers),freq = Freq_gainers,
          min.freq = 1,random.order = F, color = brewer.pal(8,"Dark2"))



# Losers wordcloud 

wordcloud(words = names(Freq_losers),freq = Freq_losers,
          min.freq = 4,random.order = F, color = brewer.pal(8,"Dark2"))


# Twitter Sentiment Function

sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  #calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if(p == 0 & n == 0){
    return (NA)
  }
  else {
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (p-n)
  }
}


pos.words <- scan("positive-words.txt",what= "character",comment.char = ";")

neg.words <- scan("negative-words.txt",what= "character",comment.char = ";")

 save(list = (c("pos.words","neg.words")),file = "sentimentWords")



gainersText <- lapply(tweets_gainer, function(t){
  iconv(t$getText(),"latin1","ASCII",sub = "")
})

losersText <- lapply(tweets_losers, function(t){
  iconv(t$getText(),"latin1","ASCII",sub = "")
})

score_gainer <- sapply(gainersText,sentiment,pos.words,neg.words)

gain_df <- as.data.frame(table(score_gainer))
gain_df


score_loser <- sapply(losersText,sentiment,pos.words,neg.words)
lose_df <- as.data.frame(table(score_loser))
lose_df

# Gainer Score Plot 
ggplot(gain_df,aes(x = score_gainer,y = Freq,fill = score_gainer)) + geom_bar(stat = "identity") +xlab("Scores")

# Loser Score Plot 
ggplot(lose_df,aes(x = score_loser,y = Freq,fill = score_loser)) + geom_bar(stat = "identity") + xlab("Scores")



