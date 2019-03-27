setwd("C:\\Users\\Bharathyramakrishnan\\Documents\\R Prog\\ExcelR\\Excelr Data-\\Excelr Data\\Assignments\\Text Mining")
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
# Amazon Reviews of HP 23.8-inch FHD IPS Monitor with Tilt/Height Adjustment and Built-in
aurl <- "https://www.amazon.com/Acer-G276HL-Kbix-Frame-Monitor/product-reviews/B0742D9CDX/ref=cm_cr_getr_d_show_all?ie=UTF8&reviewerType=all_reviews&pageNumber=1&pageNumber"
#https://www.amazon.com/Samsung-Galaxy-S9-Unlocked-Smartphone/product-reviews/B079H6L4V2/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=21&pageNumber"
#https://www.amazon.com/Acer-G276HL-Kbix-Frame-Monitor/product-reviews/B0742D9CDX/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"
#https://www.amazon.com/HP-23-8-inch-Adjustment-Speakers-VH240a/product-reviews/B072M34RQC/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"
#https://www.amazon.com/MSI-GT63-TITAN-052-Extreme-i7-8750H/product-reviews/B07CSFW5Y1/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)

write.table(amazon_reviews,"Acermonitor.txt",row.names = F)

Acermoni<-read.delim('Acermonitor.txt')

str(Acermoni)
View(Acermoni)
# Build Corpus and DTM/TDM
library(tm)
corpus <- AcerTV[-1,]
head(corpus)

class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
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
cleanset<-tm_map(cleanset,removeWords, c('tv','can','hp'))
# Since the word tv and can is used more, this can be removed as we are 
# mining the tweets for this TV and hp only.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.
cleanset <- tm_map(cleanset, gsub,pattern = 'tv', replacement = 'machine')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))
#monitor have high frequency, This implies review about the monitor's performance 

#word cloud 
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

letterCloud(w,word ='screen' ,frequency(5), size=1)

# Sentiment Analysis
Amzn_reviews <- read.delim('Acermonitor.txt')
reviews <- as.character(Amzn_reviews[-1,])
class(reviews)
s <- get_nrc_sentiment(reviews)
head(s)
#review 2 have 5 anger,17 anticipation,5 disgust,9 fear,14 joy,14 sadness,9 surprise, 23 trust, 21 negative, 36 positive.
#it gives all the emotions.
reviews[2]
get_nrc_sentiment('good')
#good has 1 anticipation, 1 joy, 1 surprise, 1 trust, 1 positive
get_nrc_sentiment('glaring')  #1 Anger and 1 Negavite


#overall the Monitor is good. positive reviews.
# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        -HP-23-8-inch-Adjustment-Speakers-VH240a')

