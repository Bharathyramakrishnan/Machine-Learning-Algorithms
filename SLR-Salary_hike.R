#4. Salary_hike -> Build a prediction model for Salary_hike

#Both X & Y are Continuous Variable.
# Reading the data from data file and saving into a variable
Sala_Hike <-read.csv(file.choose())
# Exploratory data analysis

summary(Sala_Hike)
# Variance and Standard deviation of Salary column
var(Sala_Hike$Salary) #751550960
sd(Sala_Hike$Salary) #27414.43

# Variance and Standard deviation of YearsExperience column

var(Sala_Hike$YearsExperience) # 8.053609
sd(Sala_Hike$YearsExperience) #2.837888

# Scatter Plot
plot(Sala_Hike)
attach(Sala_Hike)
cor(YearsExperience, Salary)             # cor(X,Y)
# Creating Linear Model for Salary Hike
SH<-lm(Salary ~ YearsExperience, data = Sala_Hike)
summary(SH)

#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554 p-value: < 2.2e-16

plot(SH)
confint(SH,level=0.95)

predict(SH,interval="predict")

influence.measures(SH)
influenceIndexPlot(SH,id.n=3)
influencePlot(SH,id.n=3)


# Removing the influence obs  24
SH1<-lm(Salary ~ YearsExperience, data = Sala_Hike[c(24),])
summary(SH1)
#ALL 1 residuals are 0: no residual degrees of freedom!

#So lets consider all the observations 

#Hence the P-value 2.2e-16  is less than 0.05. 
#So X variable is significance and also Multiple R-Square value is 0.957
#That's mean this model will predict the output 95.7% time correct