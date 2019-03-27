library(arules)
library(arulesViz)
mymovies <- read.csv(file.choose())
summary(mymovies)
View(mymovies)
# Col 1 to 5 are transcational data so removing those columns
rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=2)))
## Provided the rules with 2% Support, 50 % Confidence and watched a minimum of 2 movies
rules
inspect(head(sort(rules, by = "lift")))  

head(quality(rules))

plot(rules,method = "scatterplot") #or
rules %>% plot()

rules %>% plot(method="grouped")

# It looks ike most of them has watched Lord of the rings movies along with Gladiator and Greenmile
# Also most of them has watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator. 
plot(rules,method = "graph")
rules %>% plot(method="graph", control=list(layout=igraph::with_fr()))
rules %>% plot(method="graph", control=list(layout=igraph::in_circle()))
rules %>% plot(method="paracoord",control=list(reorder=T))

rules %>% plot(interactive = T)
