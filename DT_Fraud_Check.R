#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
FraudCheck <- read.csv(file.choose())
str(FraudCheck)
View(FraudCheck)
hist(FraudCheck$Taxable.Income)
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC <-data.frame(FraudCheck,Risky_Good)
FC_train <- FC[1:300,]
FC_test <- FC[301:600,]
png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
plot(opall_tree)
png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)
plot(op_tree)
pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)

mean(pred_test_df==FC_test$Risky_Good)
CrossTable(FC_test$Risky_Good,pred_test_df)
     
confusionMatrix(FC_test$Risky_Good,pred_test_df)
#Accuracy : 0.82 