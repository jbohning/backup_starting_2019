library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)

my_key="A7HkCdru2ABobiZVvT8puUf8u"
my_secret="JVbMVX9ldi7g0dqH5zOPRoSPziv22U2JLFT0pt4soci8ZDbBEp"
my_access_token="781156166818598912-GZwnEKYYgk7v5TVvuQjeP2KDcVsp2yi"
my_access_secret="9Q286sKD5cfgrpTVF9QRvTJl7o9SIITQGo1DI4Tqh5Z9j"

# Speficy Authentification Token's provided in your Twitter App
create_token(
    app = "APPNAME",
    consumer_key = my_key,
    consumer_secret = my_secret,
    access_token = my_access_token,
    access_secret = my_access_secret
)

#####################################
#Learn about tweets using the hashtag 'Halloween'
#####################################

#search for 500 tweets using the #halloween hashtag
halloween_tweets <- search_tweets(q = "#halloween",
                                  n = 500)

#view column with screen names - top 6
head(halloween_tweets$screen_name)

# what users are tweeting about Halloween
users <- search_users("#halloween",
                      n = 500)

#how many locations are represented
length(unique(users$location))

#See top tweets
head(halloween_tweets$text)

#####################################
#See posts made about @HRBlock
#####################################
hrb<-search_tweets(q = "@HRBlock",n = 500)





#####################################

#Create word cloud of HRB tweets

#####################################
#unest tweets
library("tm") 
library("SnowballC") 
library("wordcloud")
library("RColorBrewer") 

## Load the data as a corpus
docs<-Corpus(VectorSource(hrb$text))

##Text Transformation
toSpace<-content_transformer(function (x,pattern) gsub(pattern,"",x))
docs<-tm_map(docs,toSpace,"/")
docs<- tm_map(docs,toSpace,"@")
docs<- tm_map(docs,toSpace,"\\|")
docs<- tm_map(docs,toSpace,"\\.")

##Convert the text to lower case
docs<-tm_map(docs,content_transformer(tolower))

## remove URLs
removeURL<- function(x) gsub("http[^[:space:]]*","",x)
docs<- tm_map(docs, content_transformer(removeURL))

## Remove numbers
docs<-tm_map(docs, removeNumbers)

##Remove english common stopwords
docs<-tm_map(docs,removeWords,stopwords("en"))

## Remove punctuation
docs<-tm_map(docs,removePunctuation)

docs<-tm_map(docs,stripWhitespace)

## Build a term-document matrix (a document matrix is a table containing the 
## frequency of the words)
dtm<-TermDocumentMatrix(docs)
m<-as.matrix(dtm)
v<-sort(rowSums(m),decreasing=TRUE)
d<-data.frame(word=names(v),freq=v)
head(d,10)

## Generate the word cloud
set.seed(1234)
wordcloud(words=d$word, freq=d$freq,
          min.freq = 1, 
          max.words = 200,
          random.order = FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))


