CarEvaluationData <-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"),header=FALSE)
colnames(CarEvaluationData) <- c("Buying","Maint","Doors","Persons","Lug_boot","safety","CAR")
write.csv(CarEvaluationData,file="C:/Users/Bharathyramakrishnan/Documents/R Prog/ExcelR/Excelr Data-/UCI Datasets/Car_Evaluation_Data.csv",row.names = F)
str(CarEvaluationData)
# Data Partition
set.seed(222)
ind <- sample(2, nrow(CarEvaluationData), replace = T, prob = c(0.7, 0.3))
train <- CarEvaluationData[ind==1,]
test <- CarEvaluationData[ind==2,]

#check dimensions of train & test set
dim(train) #1210 obs 7 variables
dim(test) #518 obs 7 variables

#Preprocessing Train data
anyNA(CarEvaluationData) # Returns false no Missing Values 
summary(CarEvaluationData)

# Building model on training data 
library(caret)#Provides train() fn for training our data for various algorithms.

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # It controls the computational nuances of the train() method.
#Method hold the details ablout resampling , values are "boot", "boot632", "cv", "repeatedcv"(repeated Cross validation), "LOOCV", "LGOCV" etc.
#The "number" parameter holds the number of resampling iterations.
#The "repeats " parameter contains the complete sets of folds to compute for our repeated cross-validation. 

set.seed(3333)
#?rpart #Recursive Partitioning and Regression Trees , specifically available for decision tree implementation. 

dtree_fit <- train(CAR ~., data = train, method = "rpart",
                   parms = list(split = "information"), #for information gain we need add split = informataion ofr gini index we need to add gini
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit # accuracy metrics for different values of cp, cp is complexity parameter for our dtree.

#Plot Decision Tree
library(rpart)
library(rpart.plot)
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2) 
# It shows the attribute's selection order for criterion as information gain.

#Prediction
predict(dtree_fit, newdata = test[1,]) # first record prediction 

test_pred <- predict(dtree_fit, newdata = test)
test_pred
confusionMatrix(test_pred, test$CAR )  #check accuracy 
#Accuracy : 0.8591

#gini index criterion
set.seed(3333)
dtree_fit_gini <- train(CAR ~., data = train, method = "rpart",
                          parms = list(split = "gini"),
                          trControl=trctrl,
                          tuneLength = 10)
dtree_fit_gini #cp = 0.008219178.

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)
test_pred_gini <- predict(dtree_fit_gini, newdata = test)
confusionMatrix(test_pred_gini, test$CAR )  #check accuracy
#Accuracy : 0.861 

#rpart
library(rpart)
library(rpart.plot)
fit <- rpart(CAR~., data = train, method = 'class')
rpart.plot(fit, extra = 106)
c <-predict(fit, test, type = 'class')
table_mat <- table(test$CAR, predict)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test)) 
#Accuracy :0.93
#CrossTable(test$CAR,c)

#C5.0
library(C50)
car_train <-  C5.0(train[,-7],train$CAR)
plot(car_train)
mean(train$CAR==predict(car_train,train)) # 98.18% Accuracy
predc5.0_test <- predict(car_train,newdata=test) # predicting on test data
predc5.0_test
mean(predc5.0_test==test$CAR) # 96.71% accuracy 

library(gmodels)
CrossTable(test$CAR,predc5.0_test)

#Using tree function 
library(tree)
car_train_tree <- tree(CAR~.,data=train)
plot(car_train_tree)
text(car_train_tree,pretty = 0)

# Predicting the test data using the model
#pred_tree <- as.data.frame(predict(car_train_tree,newdata=test))
#pred_tree["final"] <- NULL
#pred_tree
#table(test$CAR)
#for (i in 1:nrow(pred_tree)){
 # pred_tree[i,"final"]<-ifelse(pred_tree[i,"unacc"]>0.5,"unacc",ifelse(pred_tree[i,"acc"]>0.5,"acc",ifelse(pred_tree[i,"good"]>0.5,"good","vgood"))
#}
#mean(pred_tree$final==test$CAR) # Accuracy = 94.66%
#CrossTable(test$CAR,pred_tree$final)

#pred_tree <- predict(car_train_tree,newdata=test) # predicting on test data
#pred_tree
#mean(pred_tree==test$CAR)
#CrossTable(test$CAR,pred_tree)
