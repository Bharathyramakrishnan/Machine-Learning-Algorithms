library(arules)
library(arulesViz)
getwd()
setwd("C:\\Users\\Bharathyramakrishnan\\Documents\\R Prog\\ExcelR\\Excelr Data-\\Excelr Data\\Assignments\\Association Rules")
#phone <- read.table(file="phonedata.txt",sep="\t")
phone <-  read.transactions("phonedata.txt",format="basket",sep=",")
View(phone)
summary(phone)
inspect(phone[1:10])
class(phone)
itemFrequencyPlot(phone)
rules<-apriori(phone,parameter=list(support=0.003, confidence = 0.5,minlen=3))
plot(rules)

rule1<-apriori(phone)

library(magrittr)

rules %>% plot()
rules %>% plot(method="grouped")


rules %>% plot(method="graph", control=list(layout=igraph::with_fr()))
rules %>% plot(method="graph", control=list(layout=igraph::in_circle()))
rules %>% plot(method="paracoord",control=list(reorder=T))

#Interactive Plots and Reorder rules
rules %>% plot(interactive = T)


