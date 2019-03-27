library(randomForest)
library(MASS)
library(caret)
set.seed(123)
FraudCheck <- read.csv(file.choose())
View(FraudCheck)
str(FraudCheck)
hist(FraudCheck$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
View(FCtemp)
FC = FCtemp[,c(1:7)]
str(FC)
table(FC$Risky_Good)   # 476 good customers and 124 risky customers
# Data Partition
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
rf <- randomForest(Risky_Good~., data=train)
rf
# each tree node.
# Out of bag estimate of error rate is 0.47 % in Random Forest Model.

attributes(rf)

# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$Risky_Good)

confusionMatrix(pred1, train$Risky_Good) # 100 % accuracy on training data 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) # 100 % accuracy on test data 

# Error Rate in Random Forest Model :
plot(rf)
# at 200 there is a constant line and it doesnot vary after 200 trees


# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)  # 100 % accuracy on training data 

# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good)  # 100 % accuracy on test data 

# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")

# Majority of the trees has an average number of more than 80 nodes. 

# Variable Importance :

varImpPlot(rf1)

# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say Taxable.Income is the most important variable for prediction.on looking at City population,it has no value.

# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Taxable.Income is very important and Urban is not that important.

varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")

# Quantitative values 
importance(rf1)

varUsed(rf)
# Partial Dependence Plot 
partialPlot(rf1, train, Taxable.Income, "Good")
# On that graph, i see that if the taxable Income is 30000 or greater,
# than they are good customers else those are risky customers.
# Extract single tree from the forest :
tr1 <- getTree(rf1, 2, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)
