#Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
#Draw the inferences from the clusters obtained.

#Read File
library(data.table)
library(readxl)
setwd("C:/Users/Bharathyramakrishnan/Documents/R Prog/ExcelR/Excelr Data-/Excelr Data/Assignments/Clustering")
#datasets <- system.file("EastWestAirlines.xlsx", package = "readxl")
#read_excel
clust_Air <- read_excel("EastWestAirlines.xlsx",2)
View(clust_Air)
str(clust_Air)
# Since ID# is sequence number, so skipping that column.
Clust_Airdata <- clust_Air[,2:12]

# Using Scale fn Normalizing the Data 
normalized_data<-scale(Clust_Airdata)
#Build the Distance matrix with euclidean method
d <- dist(normalized_data, method = "euclidean") 

d
# Hierarchical clustering 
fit <- hclust(d, method="complete")
fit

plot(fit)
plot(fit, hang=-1)
groups <- cutree(fit, k=5)
#rect.hclust(fit, k=2, border="red")
table(groups)

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(clust_Air, membership)
# <- final[,c(ncol(final),1:(ncol(final)-1))] # Move membership column to first 
# or using setcolorder we can move the membership column to first
setcolorder(final,c("membership"))
#write.xlsx(final, file="final.xlsx")
g1 = aggregate(clust_Air[,2:11],list(groups),median)
data.frame(Cluster=g1[,1],Freq=as.vector(table(groups)),g1[,-1])

## Hierarchical clustering using Ward's method
#library(dendextend)
#res.hc    <- hclust(d, method = "ward.D2" )
#res.hc <- as.dendrogram(res.hc)
#cd = color_branches(res.hc,k=3)
#plot(cd) # display dendrogram
#plot(res.hc, hang=-1)

write.csv(final,"C:/Users/Bharathyramakrishnan/Documents/R Prog/ExcelR/Assignments/Airlines_clust.csv")



#KMeans
library(plyr)
#Read File
EWA <- read_xlsx("EastWestAirlines.xlsx",2)
str(EWA)
#Normalize the data 
normalized_data<-scale(EWA[,2:12])

#Elbow Curve to determine the k clusters
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# Based on the elbow curve we can decide 3 clusters
fit <- kmeans(normalized_data, 3)
final2<- data.frame(EWA, fit$cluster) # append cluster membership
aggregate(EWA[,2:12], by=list(fit$cluster), FUN=mean)
#to check which data point belongs to which cluster
table(fit$cluster)

library(animation)
Al<- kmeans.ani(normalized_data, 3)

#Cluster Stability - K-means - Silhouette
library(ggplot2)
library(cluster)
library(factoextra)
km.3 <- eclust(EWA[,2:11], "kmeans", k = 3, nstart = 25, graph = TRUE)
fviz_silhouette(km.3)
#Average Silhouette width is 0.63
km.3 <- eclust(EWA[,2:11], "pam", k = 3, graph = TRUE)
fviz_silhouette(km.3)
#Average Silhouette width is 0.5
