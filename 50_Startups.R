#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and make a table containing R^2 value for each prepared model.
#Output->Continuous - Profit, Input - > X - Multiple - R&D Spend,Administration,Marketing Spend State
#Continuous Output and Multiple Input - GO with Multiple Linear Regression model

# Reading csv File for 50_Startups.
Startups <- read.csv(file.choose())
class(Startups)
View(Startups)
#State is categorical data buy using Dummies pakage convert in to Numerical
library(dummies)
Startups1<-cbind(Startups,dummy(Startups$State,sep = "-"))
Startups1<- as.data.frame(Startups1[-4])
View(Startups1)
colnames(Startups1)

library(corpcor)
cor2pcor(cor(Startups1))

library(psych)
pairs.panels(Startups1)

# Otherway to split the categorical data in to numeric.using Plyr pkg.
library(plyr)
Startups$State <- revalue(Startups$State,c("New York"="0", "California"="1", "Florida"="2"))
attach(Startups)
Startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)
Startups <- as.data.frame(Startups)
attach(Startups)

# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot

summary(Startups)

# To find the correlation betwn profit and other variables using scatter plot.
plot(R.D.Spend, Profit)# Direction +Ve, Strength - Moderate, Relation - Linear.
plot(Administration,Profit) # No Correlation
plot(Marketing.Spend, Profit)# Direction +Ve, Strength - Weak, Relation - Linear.
plot(State, Profit) # No Correlation

# Find the correlation b/n Output (profit) & (R&D Spend,Administration,Marketing Spend, State)
pairs(Startups) # Profit and RD_Spend have high corelation 

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Startups) # 0.972 high correlation betwn Profit and RD_Spend

# Partial Correlation matrix - Pure Correlation b/n the variables
library(corpcor)
cor2pcor(cor(Startups))

library(psych)
pairs.panels(Startups) #Before removing State variable

# Below code is also useful to check the Scatter plot and correlation
### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

#Strength & Direction of Correlation is unable to identify with State Variable so we try to remove and check the correlation between other variables 

Startups[4]<-NULL
cor(Startups)
library(corpcor)
cor2pcor(cor(Startups))

# Building Linear Model of Interst.
Startups_LM <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=Startups)
summary(Startups_LM) # Multiple R-squared:  0.9507, p-value: < 2.2e-16 
# When we check the P-value for Individual variable Administration, Marketing_Spend and State have P-value > 0.05

Startups_LM1 <- lm(Profit~RD_Spend+Administration+Marketing_Spend+Startups-California+Startups-Florida+Startups-New York,data=Startups1)
summary(Startups_LM1) #Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9452 
#p-value: < 2.2e-16

# It is better to delete a single observation rather than entire variable to get rid of collinearity problem
# Deletion Diagnostics for identifying influential variable

library(mvinfluence)
library(car)
influence.measures(Startups_LM)
influenceIndexPlot(Startups_LM, id.n=3) # Index Plots of the influence measures
# Any observation which goes beyound 0.5 in Cook's distance it is influence observation.
# Using Diagnostic Plots it comes to know that observation 46,47,49 and 50 are influence varibale.

# A user friendly representation of the above
influencePlot(Startups_LM, id.n=3) 

### Variance Inflation Factors is a formal way to check for collinearity
vif(Startups_LM)  # VIF is > 10 => collinearity

avPlots(Startups_LM, id.n=2, id.cex=0.7) # Added Variable Plots

## Regression after deleting the 49th and 50th observation, which is influential observation
Startups_LMR <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=Startups[-c(49,50),])
summary(Startups_LMR) # Multiple R-squared:  0.9627 ,  p-value: < 2.2e-16
# Since we remove influence observation we need to consider the Adjusted R-squared.Adjusted R-squared:  0.9593 

# R- Squared Value is increated after removing the Influence observation. Lets consider this model.
# CI for Startups_LMR 
confint(Startups_LMR ,level=0.95)

Profit_predict <- predict(Startups_LMR,interval="predict")
View(Profit_predict)

#Final <- cbind(Startups$RD_Spend,Startups$Administration,Startups$Marketing_Spend,
 #              Startups$State,Startups$Profit,Profit_Predict)
#View(Final)
hist(residuals(Startups_LMR)) # close to normal distribution

# Evaluate model LINE assumptions
plot(Startups_LMR)

qqPlot(Startups_LMR, id.n=5) # QQ plots of studentized residuals, helps identify outliers
library("MASS")
stepAIC(Startups_LMR) # Lower AIC Value Better is the Model.
