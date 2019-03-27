#3.Consider only the below columns and prepare a prediction model for predicting Price.
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
#Output->Continuous - price, Input - > X - Multiple - Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight
#Continuous Output and Multiple Input - GO with Multiple Linear Regression model

#Reading csv File for Corolla
Corolla <- read.csv(file.choose())
attach(Corolla) # to Manipulate the specific dataset.
# There are 38 variables but we just considering only 9 so binding the required variables
Corolla1 <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
Corolla_1 <- as.data.frame(Corolla1)
class(Corolla_1)
View(Corolla_1)
# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(Corolla_1)
attach(Corolla_1)# To avoid the refrence of the dataset
windows()
# Find the correlation between Output (Price) & inputs (speed,hd,ram,screen,cd,multi,premium,ads,trend) - SCATTER DIAGRAM
pairs(Corolla_1)

# Correlation coefficient - Strength & Direction of correlation
cor(Corolla_1)
#Quarterly_Tax and Weight moderate correlated

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Corolla_1))

library(psych)
pairs.panels(Corolla_1)

# The Linear Model of interest with all the variables
cp <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Corolla_1)
summary(cp)
#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863 , p-value: < 2.2e-16
#But Individual p-values for cc and doors are insignificant 

# delete cc and doors 
cpr <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight,data = Corolla_1)
summary(cpr)
#To check wheather any influence obs affect the R-squared
library(mvinfluence)
library(car)
influence.measures(cp)
influenceIndexPlot(cp, id.n=3) # Index Plots of the influence measures
influencePlot(cp, id.n=3)
# 81 is the influence observation so we remove and check the R-squared value.

cp1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla_1[-81,])
summary(cp1)
#Multiple R-squared:  0.8694,	Adjusted R-squared:  0.8686 
#p-value: < 2.2e-16

vif(cp1)  # VIF is > 10 => collinearity
avPlots(cp1, id.n=2, id.cex=0.7) # Added Variable Plots

finalmodel <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Corolla_1)
summary(finalmodel)
#Multiple R-squared:  0.8636,	Adjusted R-squared:  0.863 , p-value: < 2.2e-16
#Since cp model gives high R-squared value we consider that model and predict

confint(cp,level=0.95)
Price_Predict <- predict(cp,interval="predict")
Pred_final <- predict(cp)


# Evaluate model LINE assumptions
plot(cp)

#View(Final)
hist(residuals(cp)) # close to normal distribution

# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot((cp),id.n=5) # QQ plots of studentized residuals, helps identify outliers
#81 is oulier

library("MASS")
stepAIC(cp)


