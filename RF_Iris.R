library(randomForest)
library(MASS)
library(caret)
set.seed(123)
data(iris)
View(iris)
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

rf <- randomForest(Species~., data=iris_train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
# each tree node.
# Out of bag estimate of error rate is 4 % in Random Forest Model.
attributes(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, iris_train)
head(pred1)
head(iris_train$Species)
# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, iris_train$Species)  # 100 % accuracy on training data 
# Prediction with test data - Test Data 
pred2 <- predict(rf, iris_test)
confusionMatrix(pred2, iris_test$Species) 
plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(iris_train[,-5], iris_train[,5], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(Species~., data=iris_train, ntree = 140, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, iris_train)
confusionMatrix(pred1, iris_train$Species)

pred2 <- predict(rf1, iris_test)
confusionMatrix(pred2, iris_test$Species)


# no of nodes of trees
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")

# Variable Importance :
varImpPlot(rf1)
# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say Petal Length is the most important variable for prediction.on looking at Sepat.Width.has least score
# to be used for predication
# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Petal.Width is very important and Sepal.Width is not that important.

varImpPlot(rf1 ,Sort = T, n.var = 4, main = "Top 4 -Variable Importance")
# Quantitative values 
importance(rf1)
varUsed(rf)   # which predictor variables are actually used in the random forest.
# Partial Dependence Plot 
partialPlot(rf1, iris_train, Petal.Length, "versicolor")
partialPlot(rf1, iris_train, Petal.Length, "setosa")
partialPlot(rf1, iris_train, Petal.Length, "virginica")

# if the petal.length is between 2.5 to 5,5, then it is Versicolor
# If the petal.length is between 1 to 3 cms in length, then it is setosa
# if the petal.length is greater than 3 cms in lenth, then it is Virginica
# Extract single tree from the forest :

tr1 <- getTree(rf1, 1, labelVar = TRUE)
tr1

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, iris$Species)
