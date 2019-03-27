#2. Predict sales of the computer
#Prepare a prediction model to predict the price of the computer
#Output->Continuous - price, Input - > X - Multiple - speed,hd,ram,screen,cd,multi,premium,ads,trend
#Continuous Output and Multiple Input - GO with Multiple Linear Regression model

#Reading csv File for Compuder_data
Comdata <- read.csv(file.choose())
class(Comdata)
View(Comdata)
# Cd, Multi and Premium are Categorical Data so converting it into Dummy Variables or Numeric.
library(plyr)
Comdata1 <- Comdata
Comdata1$cd <- as.numeric(revalue(Comdata1$cd,c("yes"="1", "no"="0")))
Comdata1$multi <- as.numeric(revalue(Comdata1$multi,c("yes"="1", "no"="0")))
Comdata1$premium <- as.numeric(revalue(Comdata1$premium,c("yes"="1", "no"="0")))
View(Comdata1)
class(Comdata1)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(Comdata1)
attach(Comdata1) # To avoid the refrence of the dataset 

# To find the correlation betwn Price and other variables using scatter plot.
plot(speed, price) 
#plot(hd, price)
#plot(ram, price)
#plot(screen, price)
#plot(cd, price)
#plot(multi, price)
#plot(premium, price)
#plot(ads, price)
#plot(trend, price)

windows()
# Find the correlation between Output (Price) & inputs (speed,hd,ram,screen,cd,multi,premium,ads,trend) - SCATTER DIAGRAM
pairs(Comdata1)
cor(Comdata1)
# Hd and Cd have Moderate Correlation,Price and ram have moderate correlation, Similary trend and hd also have moderate correlation.

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Comdata1))

library(psych)
pairs.panels(Comdata1)

# The Linear Model of interest with all the variables
Comdata_lm <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(Comdata_lm)
#Multiple R-squared:  0.7756, p-value: < 2.2e-16 and all other individual p-value is less than 0.05

# Multicollinearity check
Comdata_lm_Spd <- lm(price~speed)
summary(Comdata_lm_Spd) #Speed became significant
Comdata_lm_hd <- lm(price~hd)
summary(Comdata_lm_hd) #hd became significant
Comdata_lm_ram <- lm(price~ram)
summary(Comdata_lm_ram) #hd became significant

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(Comdata_lm) # Original model
## vif>10 then there exists collinearity among all the variables

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Comdata_lm,id.n=2,id.cex=0.7)

# To check the influence observations
library(mvinfluence)

library(car)
influence.measures(Comdata_lm)
influenceIndexPlot(Comdata_lm, id.n=3) # Index Plots of the influence measures
# 1441, 1701,3784 and 4478 seems to be influence observation

influencePlot(Comdata_lm, id.n=3)# To confirm the influence obs
# when we check with the values studentized Residuals 0bs 1441 and 1701 are influence obs 
# so we remove those values and check wheather R-squared value is increasing or not

#Regression after deleting the 1441 and 1701 observation, which is influential observation
Comdata_lmr <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=Comdata1[-c(1441,1701),])
summary(Comdata_lmr)

#Multiple R-squared:  0.7777,	Adjusted R-squared:  0.7774 Since we removed 2 obs we also consider adjusted R-squared value.
#Even though there is no much difference in R- Squared value so we try with Transformations

# Logarthimic Transformation 
Comdata_Log <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=Comdata1[-c(1441,1701),])
summary(Comdata_Log) 
#Multiple R-squared:  0.7445,	Adjusted R-squared:  0.7441 , p-value: < 2.2e-16
# R-squared is reduced 

# Poly Modal
Comdata_Poly <- lm(price~speed+I(speed^2)+I(speed^3)+
                            hd+I(hd^2)+I(hd^3)+
                            ram+I(ram^2)+I(ram^3)+
                            screen+I(screen^2)+I(screen^3)+
                            cd+I(cd^2)+I(cd^3)+
                            multi+I(multi^2)+I(multi^3)+
                            premium+I(premium^2)+I(premium^3)+
                            ads+I(ads^2)+I(ads^3)+
                            trend+I(trend^2)+I(trend^3),data=Comdata1[-c(1441,1701),])
summary(Comdata_Poly)
#Multiple R-squared:  0.8136,	Adjusted R-squared:  0.813 , p-value: < 2.2e-16
# R-Squared value is incresed.

avPlots(Comdata_Poly, id.n=2, id.cex=0.7) # Added Variable Plots

# Final Model
FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3),data=Comdata1[-c(1441,1701),])

summary(FinalModel)
confint(FinalModel,level=0.95)
predict(FinalModel,interval="predict")
predict(FinalModel)

# Evaluate model LINE assumptions
plot(FinalModel)

#View(Final)
hist(residuals(FinalModel)) # close to normal distribution

# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot((FinalModel),id.n=5) # QQ plots of studentized residuals, helps identify outliers

library("MASS")
stepAIC(FinalModel) # backward
