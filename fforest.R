library(neuralnet)  
library(nnet)
library(NeuralNetTools)
library(dplyr)
# Read the data
fforest <- read.csv(file.choose())
fforest<-fforest[3:30]
View(fforest)
str(fforest)

# The area value has lots of zeros
hist(fforest$area)
rug(fforest$area)
# Transform the Area value to Y 

fforest1 <- mutate(fforest, y = log(area + 1))  # default is to the base e, y is lower case
hist(fforest1$y)
summary(fforest) # Confirms on the different scale and demands normalizing the data.

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
fforest_norm<-as.data.frame(lapply(fforest,FUN=normalize))
summary(fforest_norm$area)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(fforest_norm), replace = TRUE, prob = c(0.7,0.3))
fforest_train <- fforest_norm[ind==1,]
fforest_test  <- fforest_norm[ind==2,]
# Creating a neural network model on training data

fforest_model <- neuralnet(area~.,data = fforest_train)
str(fforest_model)
plot(fforest_model, rep = "best")
summary(fforest_model)
par(mar = numeric(4), family = 'serif')
plotnet(fforest_model, alpha = 0.6)

# Evaluating model performance

set.seed(12323)
library(data.table)
setcolorder(fforest_test,c("area"))
View(fforest_test)
model_results <- neuralnet::compute(fforest_model,fforest_test[2:28])
predicted_area <- model_results$net.result
# Predicted strength Vs Actual Strength of test data.
cor(predicted_area,fforest_test$area)

#Skipping the days and months 
fforest_model1 <- neuralnet(area~FFMC+DMC+DC+ISI+temp+RH+wind+rain,data = fforest_train)
str(fforest_model)
plot(fforest_model1, rep = "best")
summary(fforest_model1)
par(mar = numeric(4), family = 'serif')
plotnet(fforest_model1, alpha = 0.6)

# Evaluating model performance

set.seed(12323)
setcolorder(fforest_test,c("area"))
View(fforest_test)
model_results1 <- neuralnet::compute(fforest_model,fforest_test[2:9])
predicted_area1 <- model_results1$net.result
# Predicted strength Vs Actual Strength of test data.
cor(predicted_area1,fforest_test$area)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on strength.
str_max <- max(fforest$area)
str_min <- min(fforest$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)
# Improve the model performance :
set.seed(12345)
fforest_model2 <- neuralnet(area~FFMC+DMC+DC+ISI+temp+RH+wind+rain,data= fforest_train,
                             hidden = 5)
plot(fforest_model2, rep = "best")
summary(fforest_model2)
model_results2<-compute(fforest_model2,fforest_test)

predicted_area2<-model_results2$net.result
cor(predicted_area2,fforest_test$area)
plot(predicted_area2,fforest_test$area)
par(mar = numeric(4), family = 'serif')
plotnet(fforest_model2, alpha = 0.6)
#SSE(Error) has been  reduced and training steps had been increased as the number of neurons # under hidden layer are increased