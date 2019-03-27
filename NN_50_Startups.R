#Build a Neural Network model for 50_startups data to predict profit 
library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)
# Read the data
Startups <- read.csv(file.choose())
View(Startups)
class(Startups)
#State is categorical data buy using Dummies pakage convert in to Numerical
library(dummies)
Startups1<-cbind(Startups,dummy(Startups$State,sep = "-"))
Startups1<- as.data.frame(Startups1)
View(Startups1)
colnames(Startups1)
#Removing the space from the  
library(stringr)
names(Startups1)<-names(Startups1)%>% stringr::str_replace_all("\\s","_")

# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(Startups1)
# Correlation coefficient - Strength & Direction of correlation
cor(Startups1)
summary(Startups1) # Confirms on the different scale and demands normalizing the data.
library(corpcor)
cor2pcor(cor(Startups1))
library(psych)
pairs.panels(Startups1)

# Apply Normalization technique to the whole dataset :
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups1,FUN=normalize))
summary(Startups_norm$Profit) # Normalized form of profit
summary(Startups1$profit)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]

# Rename a column in R
library(data.table)
class(Startups_train)
Startups_train$
colnames(Startups_train)
#startups_model <- neuralnet(Startups_train$Profit~Startups_train$R.D.Spend+Startups_train$Administration+Startups_train$Marketing.Spend+Startups_train$Startups.California+Startups_train$Startups.Florida+Startups_train$Startups.New_York,data = Startups_train)
startups_model<-neuralnet(Profit~.,data=Startups_train)
startups_model
plot(startups_model, rep = "best")

summary(startups_model)
par(mar = numeric(4), family = 'serif')

plotnet(startups_model, alpha = 0.6)

# Evaluating model performance
set.seed(12323)
setcolorder(startups_test,c("Profit"))
View(startups_test)
model_results <- compute(startups_model,startups_test)
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(Startups1$Profit)
str_min <- min(Startups1$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)
# Improve the model performance :
set.seed(12345)
Startups_model2 <- neuralnet(Profit~.,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")
summary(Startups_model2)
model_results2<-compute(Startups_model2,startups_test)
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
plot(predicted_Profit2,startups_test$Profit)

par(mar = numeric(4), family = 'serif')
plotnet(Startups_model2, alpha = 0.6)

# SSE(Error) has reduced and training steps had been increased as the number of neurons  under hidden layer are increased