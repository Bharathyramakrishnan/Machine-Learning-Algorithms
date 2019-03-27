library(arules)
library(arulesViz)
library(colorspace)
groceries<-read.transactions(file.choose(),format="basket")
summary(groceries)
#Observations
#=============
#density of 0.0005240481 ie.0.052% no zero Matrix cells
#Matrix has 9835 times 6928 times 0.0005240481. Hence 35707 items purchsed
#Bag frquency is 971 out of 9835 transactions , means 0.0987% transaction
#Avg(Mean) = 35707/9835 = 3.63 items purchased 
#A total of 1380 transactions contained only a single item, while one Transaction have 21 times
#The first quartile and median purchase size are 2 and 3 items respectively, 
#implying that 25 percent of transactions contained two or fewer items and about half contained around three items.

#Using the inspect() for the sparsed matrix, the list of first 10 transactions can be seen below.
inspect(groceries[1:10])

#Find item frequency
itemFrequency(groceries[,1:5])
windows()
itemFrequencyPlot(groceries,topN=25)
#itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, support=0.02, cex.names=0.8)
image(groceries,100)

#Correlation analysis
#The lift score . Lift = 1 ??? A and B are independent . Lift > 1 ??? A and B are positively correlated . Lift < 1 ??? A and B are negatively correlated
#Firstly let us try the eclat algorithm - to see most frequent itemsets. Below we will see the list of the most common items together with their individual support.
freq.itemsets <- eclat(groceries, parameter=list(supp=0.075, maxlen=15))

inspect(freq.itemsets)

#Algorithm
rules1 <- apriori(groceries,parameter=list(support=0.002, confidence = 0.5))
# Provided the rules with 0.02 % Support, 50 % Confidence and Minimum to purchase 1 item 
summary(rules1)
#we obtained 101 rules where mean support is equal to 0.04% and
#mean confidence is 89%. These are not bad values. It means that mean rule occurrs in 0.04% transactions and its implication has 89% power.

grules2 = apriori(groceries, parameter = list(support = 0.009, confidence = 0.25, minlen = 2))
# Provided the rules with 0.09 % Support, 25 % Confidence and Minimum to purchase 2 item 
summary(grules2)
#we obtained 23 rules where mean support is equal to 16% and
#mean confidence is 56%. These are not bad values. It means that mean rule occurrs in 16% transactions and its implication has 56% power.

inspect(head(sort(rules1, by ="lift"),5))
inspect(head(sort(grules2, by ="lift"),5))
inspect(head(sort(rules1, by = "lift")))  
inspect(sort(sort(rules1, by ="support"),by ="confidence")[1:5])
head(quality(rules1))
inspect(sort(grules2, by = "lift")[1:10])
inspect(sort(rules1, by = "lift")[1:10])

library(magrittr)
subset.matrix <- is.subset(rules1, rules1)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix) >= 1
redundant %>% which()

## remove redundant rules
rules_redu <- rules1[!redundant]
#Remaining Rules
rules_redu %>% inspect()

#Visualizing Association Rules
rules1 %>% plot()
rules1 %>% plot(method="grouped")
rules_redu %>% plot(method="grouped")
plot(rules1, measure=c("support", "lift"), shading="confidence")


rules_redu %>% plot(method="graph", control=list(layout=igraph::with_fr()))
rules_redu %>% plot(method="graph", control=list(layout=igraph::in_circle()))
rules_redu %>% plot(method="paracoord",control=list(reorder=T))
#Interactive Scattar plot
plot(rules1, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

#Matrix based visulaization
plot(grules2, method="matrix", measure="lift")


