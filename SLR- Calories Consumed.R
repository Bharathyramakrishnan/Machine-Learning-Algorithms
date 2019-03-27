#Calories_consumed-> predict weight gained using calories consumed
#Both X & Y are Continuous Variable.

# Read File
Calories_consumed <-read.csv(file.choose())

# Exploratory data analysis
summary(Calories_consumed)

# Variance and Standard deviation of Calories. Consumed column
var(Calories_consumed$Calories.Consumed)
sd(Calories_consumed$Calories.Consumed)


# Variance and Standard deviation of Weight.gained..grams. column
var(Calories_consumed$Weight.gained..grams.)
sd(Calories_consumed$Weight.gained..grams.)

# Scatter plot
plot(Calories_consumed$Weight.gained..grams., Calories_consumed$Calories.Consumed)

# Correlation Coefficient (r)
cor(Calories_consumed$Weight.gained..grams., Calories_consumed$Calories.Consumed)  

#They are highly correlated.

# Simple Linear Regression model
WGM<-lm(Weight.gained..grams. ~ Calories.Consumed, data = Calories_consumed)
summary(WGM)

#Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
#p-value: 2.856e-07
#Hence the P-value 2.856e-07 < 0.05.
#So X variable is significance and also Multiple R-Square value is 0.8968.
#That's mean this model will predict the output 89.68% time correct.

#Try with Transformation to check whether R-squared value is increasing.
pred <- predict(WGM)
WGM$residuals

sum(WGM$residuals)
mean(WGM$residuals)
sqrt(sum(WGM$residuals^2)/nrow(Calories_consumed$Calories.Consumed))  #RMSE
sqrt(mean(WGM$residuals^2))

#confidence Interval
confint(WGM,level=0.95)

predict(WGM,interval="predict")

library(ggplot2)
 ggplot(data = Calories_consumed, aes(x = Weight.gained..grams., y = Calories.Consumed)) + 
     geom_point(color='blue') +
     geom_line(color='red',data = Calories_consumed, aes(x=Weight.gained..grams., y=pred))

 # transform the variables to check whether the predicted values are better
 attach(Calories_consumed)
 reg_sqrt <- lm(Weight.gained..grams.~ sqrt(Calories.Consumed))
 summary(reg_sqrt)

 # Multiple R-squared:  0.8567,	Adjusted R-squared:  0.8448 ,p-value: 2.083e-06
 # Above Model is better than compare to square root of calories.consumed.
 
 confint(reg_sqrt,level=0.95)
 predict(reg_sqrt,interval="predict")
 
 
 # transform the variables to check whether the predicted values are better
 reg_log1 <- lm(Weight.gained..grams.~ log(Calories.Consumed))
 summary(reg_log1)
 #Multiple R-squared:  0.8077,	Adjusted R-squared:  0.7917
 #Model without Tranformation gives better R-squared value
 confint(reg_log1,level=0.95)
 predict(reg_log1,interval="predict")
 
 ## transform the variables to check whether the predicted values are better
 reg_exp1 <- lm(log(Weight.gained..grams.)~ Calories.Consumed)
 summary(reg_exp1)
 
 #Multiple R-squared:  0.8776,	Adjusted R-squared:  0.8674 ,p-value: 8.018e-07
 #Model without Tranformation gives better R-squared value
 
 confint(reg_exp1,level=0.95)
 predict(reg_exp1,interval="predict")
 
 pred=predict(reg_exp1)
 pred1= exp(pred)
 pred1
 library(ggplot2)
 ggplot(data = Calories_consumed, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
   geom_point(color='blue') +
   geom_line(color='red',data = Calories_consumed, aes(x= Calories.Consumed, y=pred1))
 
 # Quadratic model
 quad_mod <- lm(Weight.gained..grams.~ Calories.Consumed+I(Calories.Consumed^2),data=Calories_consumed)
 summary(quad_mod)
 #Multiple R-squared:  0.9521,	Adjusted R-squared:  0.9433 
 #p-value: 5.546e-08
 #Better R- squared than previous models
 
 confint(quad_mod,level=0.95)
 predict(quad_mod,interval="predict")
 
 # Cubic model
 poly_mod <- lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3),data=Calories_consumed)
 summary(poly_mod)
 
 #Multiple R-squared:  0.9811,	Adjusted R-squared:  0.9755 , p-value: 6.395e-09
 #Better R- squared than previous models
 
 confint(poly_mod,level=0.95)
 predict(poly_mod,interval="predict")
 
 model_R_Squared_values <- list(model=NULL,R_squared=NULL)
 model_R_Squared_values[["model"]] <- c("WGM","reg_sqrt","reg_log1","reg_exp1","quad_mod","poly_mod")
 model_R_Squared_values[["R_squared"]] <- c(0.8968,0.8567,0.8077,0.8776,0.9521,0.9755)
 Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
 View(model_R_Squared_values)
 
 
 # Cubic  model gives the best Adjusted R-Squared value
 pred_final <- predict(poly_mod)
 pred_final
 
 rmse<-sqrt(mean((pred_final-Weight.gained..grams.)^2))
 rmse
 
 plot(poly_mod)
 
 hist(residuals(poly_mod)) # close to normal distribution
 
 # Result
 ## Applying poly model transformation is increased the Multiple R Squared Value. 
 #So ploy model gives better Multiple R-squared:  0.9811