install.packages("twitteR")
install.packages("ROAuth")
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("twitteR")
library("ROAuth")

consumer_key <- 'dM0aDB6vT7ojrn40Zn1Q4FwxF'
consumer_secret <- 'XsqyEaWDiqPeEVwukJy306Y3wUsbFnZG4BSbh8e1XaZeyYOQb1'
access_token <- '3343245940-bxomI44SLUE8H9puzcS4J9isl4U4PyYIprPR5nc'
access_secret <- 'dOWmPOlWQTWz2LKGIZBvk12TZuebSo0skvyYEcLHASQob'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets_g <- searchTwitter("#google", n = 1000, lang = "en")
tweets_a <- searchTwitter("#amazon", n = 1000, lang = "en")
tweets_f <- searchTwitter("#facebook", n = 1000, lang = "en")
tweets_tech <- searchTwitter("#technology", n = 1000, lang = "en")
tweets_covid <- searchTwitter("#covid19", n = 1000, lang = "en")

amazon_tweets <- twListToDF(tweets_a)
google_tweets <- twListToDF(tweets_g)
facebook_tweets <- twListToDF(tweets_f)
tech_tweets <- twListToDF(tweets_tech)
covid_tweets <- twListToDF(tweets_covid)

View(amazon_tweets)
View(google_tweets)
View(facebook_tweets)
View(tech_tweets)
View(covid_tweets)

google_text<- google_tweets$text
amazon_text<- amazon_tweets$text
facebook_text<- facebook_tweets$text
tech_text<- tech_tweets$text
covid_text <- covid_tweets$text

#convert everything to lower case
google_text<- tolower(google_text)
amazon_text<- tolower(amazon_text)
facebook_text<- tolower(facebook_text)
tech_text<- tolower(tech_text)
covid_text<- tolower(covid_text)

#remove "rt"
google_text <- gsub("rt", "", google_text)
amazon_text <- gsub("rt", "", amazon_text)
facebook_text <- gsub("rt", "", facebook_text)
tech_text <- gsub("rt", "", tech_text)
covid_text <- gsub("rt", "", covid_text)

#remove username
google_text <- gsub("@\\w+", "", google_text)
amazon_text <- gsub("@\\w+", "", amazon_text)
facebook_text <- gsub("@\\w+", "", facebook_text)
tech_text <- gsub("@\\w+", "", tech_text)
covid_text <- gsub("@\\w+", "", covid_text)

#punctuation
google_text <- gsub("[[:punct:]]", "", google_text)
amazon_text <- gsub("[[:punct:]]", "", amazon_text)
facebook_text <- gsub("[[:punct:]]", "", facebook_text)
tech_text <- gsub("[[:punct:]]", "", tech_text)
covid_text <- gsub("[[:punct:]]", "", covid_text)

#links
google_text <- gsub("http\\w+", "", google_text)
amazon_text <- gsub("http\\w+", "", amazon_text)
facebook_text <- gsub("http\\w+", "", facebook_text)
tech_text <- gsub("http\\w+", "", tech_text)
covid_text <- gsub("http\\w+", "", covid_text)

#tabs
google_text <- gsub("[ |\t]{2,}", "", google_text)
amazon_text <- gsub("[ |\t]{2,}", "", amazon_text)
facebook_text <- gsub("[ |\t]{2,}", "", facebook_text)
tech_text <- gsub("[ |\t]{2,}", "", tech_text)
covid_text <- gsub("[ |\t]{2,}", "", covid_text)

# Remove blank spaces at the beginning
google_text <- gsub("^ ", "", google_text)
amazon_text <- gsub("^ ", "", amazon_text)
facebook_text <- gsub("^ ", "", facebook_text)
tech_text <- gsub("^ ", "", tech_text)
covid_text <- gsub("^ ", "", covid_text)

# Remove blank spaces at the end
google_text <- gsub(" $", "", google_text)
amazon_text <- gsub(" $", "", amazon_text)
facebook_text <- gsub(" $", "", facebook_text)
tech_text <- gsub(" $", "", tech_text)
covid_text <- gsub(" $", "", covid_text)


library(tm)
library(tmap)
library(corpus)
library(SnowballC)
library(wordcloud)
library(stopwords)


#create corpus
google_tweets.text.corpus <- Corpus(VectorSource(google_text))
amazon_tweets.text.corpus <- Corpus(VectorSource(amazon_text))
facebook_tweets.text.corpus <- Corpus(VectorSource(facebook_text))
tech_tweets.text.corpus <- Corpus(VectorSource(tech_text))
covid_tweets.text.corpus <- Corpus(VectorSource(covid_text))


#clean up by removing stop words
google_tweets.text.corpus <- tm_map(google_tweets.text.corpus, function(x)removeWords(x,stopwords()))
amazon_tweets.text.corpus <- tm_map(amazon_tweets.text.corpus, function(x)removeWords(x,stopwords()))
facebook_tweets.text.corpus <- tm_map(facebook_tweets.text.corpus, function(x)removeWords(x,stopwords()))
tech_tweets.text.corpus <- tm_map(tech_tweets.text.corpus, function(x)removeWords(x,stopwords()))
covid_tweets.text.corpus <- tm_map(covid_tweets.text.corpus, function(x)removeWords(x,stopwords()))

library("wordcloud")
#generate wordcloud
wordcloud(google_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(amazon_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(facebook_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(tech_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(covid_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)


#getting emotions using in-built function
mysentiment_google<-get_nrc_sentiment((google_text))
mysentiment_amazon<-get_nrc_sentiment((amazon_text))
mysentiment_facebook<-get_nrc_sentiment((facebook_text))
mysentiment_tech<-get_nrc_sentiment((tech_text))
mysentiment_covid<-get_nrc_sentiment((covid_text))

#calculationg total score for each sentiment
Sentimentscores_google<-data.frame(colSums(mysentiment_google[,]))
Sentimentscores_amazon<-data.frame(colSums(mysentiment_amazon[,]))
Sentimentscores_facebook<-data.frame(colSums(mysentiment_facebook[,]))
Sentimentscores_tech<-data.frame(colSums(mysentiment_tech[,]))
Sentimentscores_covid<-data.frame(colSums(mysentiment_covid[,]))

names(Sentimentscores_google)<-"Score"
Sentimentscores_google<-cbind("sentiment"=rownames(Sentimentscores_google),Sentimentscores_google)
rownames(Sentimentscores_google)<-NULL

names(Sentimentscores_amazon)<-"Score"
Sentimentscores_amazon<-cbind("sentiment"=rownames(Sentimentscores_amazon),Sentimentscores_amazon)
rownames(Sentimentscores_amazon)<-NULL

names(Sentimentscores_facebook)<-"Score"
Sentimentscores_facebook<-cbind("sentiment"=rownames(Sentimentscores_facebook),Sentimentscores_facebook)
rownames(Sentimentscores_facebook)<-NULL

names(Sentimentscores_tech)<-"Score"
Sentimentscores_tech<-cbind("sentiment"=rownames(Sentimentscores_tech),Sentimentscores_tech)
rownames(Sentimentscores_tech)<-NULL

names(Sentimentscores_covid)<-"Score"
Sentimentscores_covid<-cbind("sentiment"=rownames(Sentimentscores_covid),Sentimentscores_covid)
rownames(Sentimentscores_covid)<-NULL

library(ggplot2)
#plotting the sentiments with scores
ggplot(data=Sentimentscores_google,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech giant GOOGLE")


ggplot(data=Sentimentscores_amazon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on ecomerce giant AMAZON")


ggplot(data=Sentimentscores_facebook,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Social Netwoking site FACEBOOK")


ggplot(data=Sentimentscores_tech,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech as a whole")

ggplot(data=Sentimentscores_covid,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on covid19 as a whole")
