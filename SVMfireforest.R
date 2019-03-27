#Prepare support vector machines model for classifying the area under fire for foresfires data

library(kernlab)
library(caret)
library(plyr)
# Read the data
SVMfforest <- read.csv(file.choose())
View(SVMfforest)
class(SVMfforest)
str(SVMfforest)
# The area value has lots of zeros
hist(SVMfforest$area)
rug(SVMfforest$area)
# Transform the Area value to Y 
SVMfforest1 <- mutate(SVMfforest, y = log(area + 1))  # default is to the base e, y is lower case
hist(SVMfforest1$y)
summary(SVMfforest) # Confirms on the different scale and demands normalizing the data.

# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
SVMfforest$temp = normalize(SVMfforest$temp)
SVMfforest$RH   = normalize(SVMfforest$RH)
SVMfforest$wind = normalize(SVMfforest$wind)
SVMfforest$rain = normalize(SVMfforest$rain)


attach(SVMfforest)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(SVMfforest), replace = TRUE, prob = c(0.7,0.3))
SVMfforest_train <- SVMfforest[ind==1,]
SVMfforest_test  <- SVMfforest[ind==2,]

# Building model 
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= SVMfforest_train,kernel = "vanilladot")
model1

Area_pred <- predict(model1, SVMfforest_test)

table(Area_pred,SVMfforest_test$size_category)

agreement <- Area_pred == SVMfforest_test$size_category
table(agreement)
prop.table(table(agreement))

pred_vanilla<-predict(model1,newdata=SVMfforest_test)
mean(pred_vanilla==SVMfforest_test$size_category)  #67.80

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= SVMfforest_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=SVMfforest_test)
mean(pred_rfdot==SVMfforest_test$size_category) #68.49

# kernal = besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= SVMfforest_train,kernel = "besseldot")
pred_besseldot<-predict(model_besseldot,newdata=SVMfforest_test)
mean(pred_besseldot==SVMfforest_test$size_category) #67.80

# kernal = polydot
model_polydot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= SVMfforest_train,kernel = "polydot")
pred_polydot<-predict(model_polydot,newdata=SVMfforest_test)
mean(pred_polydot==SVMfforest_test$size_category) #67.80

