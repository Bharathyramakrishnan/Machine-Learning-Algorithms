#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots 

library(arules)
library(arulesViz)
book <- read.csv(file.choose())

#Algorithm
#A level-wise, breadth-first algorithm which counts transactions to find frequent itemsets and then derive association rules from them
#apriori() in package arules 
#Mine frequent itemsets, association rules or association hyperedges using the Apriori algorithm.
#Default settings:
# minimum support: supp=0.1
# minimum confidence: conf=0.8
# maximum length of rules: maxlen=10

rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))
rules1 <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=10))

# Provided the rules with 2 % Support, 50 % Confidence and Minimum to purchase 5 books 

rules
inspect(head(sort(rules, by = "lift")))  

head(quality(rules))

library(magrittr) 
#It is necessary to remove redundant rules before a user is able
#to study the rules and identify interesting ones from them.

## find redundant rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix) >= 1
redundant %>% which()

## remove redundant rules
rules_redu <- rules[!redundant]
#Remaining Rules
rules_redu %>% inspect()

#Interpreting Rules based on consequent
book$CookBks%>% table() %>% prop.table()
book$ChildBks%>% table() %>% prop.table()

#Visualizing Association Rules
rules %>% plot()
rules %>% plot(method="grouped")
rules_redu %>% plot(method="grouped")

rules_redu %>% plot(method="graph", control=list(layout=igraph::with_fr()))
rules_redu %>% plot(method="graph", control=list(layout=igraph::in_circle()))
rules_redu %>% plot(method="paracoord",control=list(reorder=T))

#Interactive Plots and Reorder rules
rules_redu %>% plot(interactive = T)
#interactive = TRUE
#Selecting and inspecting one or multiple rules, Zooming, Filtering rules with an interesting measure

rules_redu %>% plot(method="paracoord", control=list(reorder=T))
#reorder = TRUE, To improve visualisation by reordering rules and minimizing crossovers, The visualisation is likely to change from run to run.

## Cook books are being sold at a larger extent along with other chld, art, geo, Doit books

