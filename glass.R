library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass <- read.csv(file.choose())
View(glass)
table(glass$Type)
#glass$type=as.factor(glass$Type)
str(glass)
normalize_data <-scale(glass[,1:9])
#Join the standardized data with the target column
data <- cbind(normalize_data,glass[10])
anyNA(data)
head(data)
corrplot(cor(data))
#caTools use to split the dataset into train and test.
set.seed(101)
sample <- sample.split(data$Type,SplitRatio = 0.70)

train <- subset(data,sample==TRUE)

test <- subset(data,sample==FALSE)

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
plot(predicted.type)

 #Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,as.factor(test$Type))
#The above results reveal that our model achieved an accuracy of 70.77 %. 
#Lets try different values of k and assess our model.

predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

#K Value by Visualization

ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

#By looking at visualtion it is clear that k=1 error is minimum,
#still we check with k=2

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=2)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,as.factor(test$Type))

#This gives Accuracy 63.08 
#so k=1 is better model
