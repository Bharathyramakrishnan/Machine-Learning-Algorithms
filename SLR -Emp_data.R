#Emp_data -> Build a prediction model for Churn_out_rate

#Both X & Y are Continuous Variable.
# Reading the data from data file and saving into a variable
Emp_data <-read.csv(file.choose())
# Exploratory data analysis
summary(Emp_data)
# Variance and Standard deviation of Salary_hike column
var(Emp_data$Salary_hike) # 8481.822
sd(Emp_data$Salary_hike) #92.09681

# Variance and Standard deviation of Churn_out_rate column
var(Emp_data$Churn_out_rate) #105.2111
sd(Emp_data$Churn_out_rate) #10.25725

#Scatter plot
plot(Emp_data$Salary_hike, Emp_data$Churn_out_rate)

attach(Emp_data)
cor(Salary_hike, Churn_out_rate)             # cor(X,Y)

# Creating Linear Model for Churn_out_rate

Ed<-lm(Churn_out_rate ~ Salary_hike, data = Emp_data)
summary(Ed)
#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101  p-value: 0.0002386

# Scatter Plot
plot(Ed)

confint(Ed,level=0.95)
predict(Ed,interval="predict")

influence.measures(Ed)
influenceIndexPlot(Ed,id.n=3)
influencePlot(Ed,id.n=3)

# Removing the influence obs  1 and 10
Ed1<-lm(Churn_out_rate ~ Salary_hike, data = Emp_data[c(-1,-10),])
summary(Ed1)
#Multiple R-squared:  0.9049,	Adjusted R-squared:  0.889 p-value: 0.0002792
plot(Ed1)

confint(Ed1,level=0.95)
predict(Ed1,interval="predict")

#Hence the P-value 0.0002792< 0.05. 
#So X variable is significance and also Multiple R-Square value is 0.9049. 
#That's mean this model will predict the output 90.49% time correct.
