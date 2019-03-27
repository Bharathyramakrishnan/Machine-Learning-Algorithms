#Extracted the Customer reviews from IMDB on the movie "FROZEN"
#and performed wordcloud and # Sentimental analysis on the same.

library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt2294629/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"Frozen.txt",row.names = F)

Frozen <- read.delim('Frozen.txt')
str(Frozen)

View(Frozen)
# Build Corpus and DTM/TDM
library(tm)
corpus <- Frozen[-1,]
head(corpus)

class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('can','film'))

cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))

# Removing the word movie and movies on similar grounds - as unnecessary.
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
cleanset <- tm_map(cleanset, stemDocument)
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)


library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()


# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.5)

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)
letterCloud(w,word = 'A',frequency(5), size=1)




IMDB_reviews <- read.delim('Frozen.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

s <- get_nrc_sentiment(reviews)
head(s)

#review 5 got allmost all the sentiments with maximum frequencey
reviews[5]

get_nrc_sentiment('splendid')
get_nrc_sentiment('no words')

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Frozen')
