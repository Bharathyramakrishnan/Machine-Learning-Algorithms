library(rmarkdown)
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
#Read Company Data File.
CompanyData <- read.csv(file.choose())
View(CompanyData)
str(CompanyData)
anyNA(CompanyData)
summary(CompanyData)
hist(CompanyData$Sales)
#Sales is normally distrinuted btwn 0 to 16 
#we make a variable "high" which is "yes" when sale is more than or equal to 10, "no" otherwise
High = as.factor(ifelse(CompanyData$Sales<10, "No", "Yes"))
CD = data.frame(CompanyData, High)
View(CD)
table(CD$High)
# No Yes 
#321  79 
head(CD)

#Splitting the data based on sales
CD_train <- CD[1:200,]
View(CD_train)
CD_test <- CD[201:400,]
View(CD_test)

#Using Party Function 
CD_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)
summary(CD_tree)
plot(CD_tree)

# Based on the visualization if the ShelveLoc is good, then probability 
#of 60% customers will buy, if ShelveLoc is bad or Medium and price <=87 then 
#their is a chance of 60% high sales, 
#if ShelveLoc is bad or medium,Price >87 and Advertisinng <=7 then 0% chance of high sales 
#if ShelveLoc is bad or Medium, Price >87 and Advertising >7 then 20% chance of high sales.

pred_tree <- as.data.frame(predict(CD_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(CD_tree,newdata=CD_test)
mean(pred_test_df==CD$High)

CrossTable(CD_test$High,pred_test_df)

confusionMatrix(CD_test$High,pred_test_df) #79.5 % Accuracy

##### Using tree function  with full dataset 
cd_tree_org <- tree(High~.-Sales,data=CD)
summary(cd_tree_org)

plot(cd_tree_org)
text(cd_tree_org,pretty = 0)

# Using the training data
cd_tree <- tree(High~.-Sales,data=CD_train)
summary(cd_tree)
plot(cd_tree)
text(cd_tree,pretty = 0)

### Evaluate the Model
# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=CD_test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(CD_test$High)
mean(pred_tree$final==CD$High)
CrossTable(CD_test$High,pred_tree$final)
confusionMatrix(CD_test$High,pred_tree$final) #86% Accuracy



