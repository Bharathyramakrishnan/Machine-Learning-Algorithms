library(arules)
library(arulesViz)
retail <- read.csv(file.choose())
View(retail)
summary(retail)

rules <- apriori(retail,parameter=list(support=0.003, confidence = 0.8,minlen=5))
rules
summary(rules)
inspect(head(sort(rules, by = "lift")))
head(quality(rules))
plot(rules,method = "scatterplot",jitter =0)
plot(rules, method = "grouped")
rules %>% plot(method="graph", control=list(layout=igraph::with_fr()))
# It looks like people who purchase Heart, Holder, T-Light and White would definitely purchase Hanging.
# People who purchase 72, Cake,Of,Pack would purchase Cases.

rules %>% plot(method="graph", control=list(layout=igraph::in_circle()))

inspect(tail(sort(rules, by = "lift")))
# There is one rule which is lift 26.93 which is less in  frequency compare to other items.