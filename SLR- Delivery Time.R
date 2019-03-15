#2. Delivery_time -> Predict delivery time using sorting time
#Both X & Y are Continuous Variable.

# Reading the data from data file and saving into a variable
delivery_time <-read.csv(file.choose())

# Exploratory data analysis
summary(delivery_time)

# Variance and Standard deviation 
var(delivery_time$Delivery.Time)
sd(delivery_time$Delivery.Time)

var(delivery_time$Sorting.Time)
sd(delivery_time$Sorting.Time)


#Scatter plot	
plot(delivery_time$Delivery.Time, delivery_time$Sorting.Time)

attach(delivery_time)
#Correlation Coefficient (r)
cor(Delivery.Time,Sorting.Time)             # cor(X,Y)
# 82.59% correlation

# Creating Linear Model for delivery time
DT<-lm(Delivery.Time ~ Sorting.Time, data = delivery_time)
summary(DT)

#Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 
#p-value: 3.983e-06
#Hence the P-value 3.983e-06< 0.05. So X variable is significance 
#And also Multiple R-Square value is 0.6823. 
#That's mean this model will predict the output 68.23% time correct.

pred <- predict(DT)
DT$residuals

sum(DT$residuals)
mean(DT$residuals)
sqrt(sum(DT$residuals^2)/nrow(Sorting.Time))  #RMSE
sqrt(mean(DT$residuals^2))

confint(DT,level=0.95)
predict(DT,interval="predict")

# ggplot for adding regression line for data
library(ggplot2)
 ggplot(data = delivery_time, aes(x = Delivery.Time, y = Sorting.Time)) + 
    geom_point(color='blue') +
     geom_line(color='red',data = delivery_time, aes(x=Delivery.Time, y=pred))
plot(DT)

#To increase the R-squared value lets check for the influence observation
library(mvinfluence)
influence.measures(DT)
influenceIndexPlot(DT,id.n=3)
influencePlot(DT,id.n=3)
# we could find the obs, 5. 9, 21 are influence obs

# Removing the influence obs and create lm model 5,9,21
DT1<-lm(Delivery.Time~Sorting.Time,data=delivery_time[c(-5,-9,-21),])
summary(DT1)

#Multiple R-squared:  0.8332,	Adjusted R-squared:  0.8228 , p-value: 1.273e-07
# we get better R-squared value, since we removing the obs we also need to consider Adjusted R-squared value also.

plot(DT1)

#After Removing 3 rows P-value 1.273e-07< 0.05. 
#So X variable is significance and
#Also Multiple R-Square value is increased to 0.8332. 
#That's mean this model will predict the output 83.32% time correct.

confint(DT1,level=0.95)
predict(DT1,interval="predict")

# transform the variables to check whether the predicted values are better
reg_log1 <- lm(Delivery.Time~ log(Sorting.Time),data=delivery_time[c(-5,-9,-21),])
summary(reg_log1)

#Multiple R-squared:  0.8378,	Adjusted R-squared:  0.8276 p-value: 1.019e-07
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")

# Cubic model
poly_mod <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3),data=delivery_time[c(-5,-9,-21),])
summary(poly_mod)

#Multiple R-squared:  0.844,	Adjusted R-squared:  0.8105 ,p-value: 6.577e-06

confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")

# Cubic  model gives the best Adjusted R-Squared value
pred_final <- predict(poly_mod)
pred_final

rmse<-sqrt(mean((pred_final-Delivery.Time)^2))
rmse

plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution

# Result
## Applying poly model transformation is increased the Multiple R Squared Value. 
#So ploy model gives better Multiple R-squared:  0.844