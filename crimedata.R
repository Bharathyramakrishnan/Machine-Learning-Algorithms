#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.
install.packages("rmarkdown")
library(rmarkdown)
#Read File
crime <- read.csv(file.choose())
View(crime)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(crime[,2:5]) #excluding the ID from spreadsheet
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")

library(dendextend)
fit <- as.dendrogram(fit)
cd = color_branches(fit,k=4)
plot(cd)
plot(fit, hang=-1)
groups <- cutree(fit, k=4)
table(groups)
Crime_Rate_Categories<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, Crime_Rate_Categories)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

View(final1)
aggregate(crime[,-1],by=list(final$Crime_Rate_Categories),mean)

#cluster 1 have maximum number of murder and Assault crime 
#cluster 2 have maximum no of rape, Assault and second place in murder and also Urbanpop
#cluster 3 have less murder but stand in second place of Urbon pop
#cluster 4 is less in all of the crimes

#Kmeans
library(plyr)
kmcrime <- read.csv(file.choose())
str(kmcrime)
# Normalize the data using scale fn
normalized_data<-scale(kmcrime[,2:5])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:5) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

#its shows after value 4 the cluster becomes constant, lets consider 4 cluster 

fit <- kmeans(normalized_data, 4) # 4 cluster solution
str(fit) #Analyzing the Cluster with various parameters like withinss, betweenss, etc, 
#analyzing which you can find out the performance of kmeans.


final2<- data.frame(kmcrime, fit$cluster) # append cluster membership
final2

aggregate(kmcrime[,2:5], by=list(fit$cluster), FUN=mean)
table(fit$cluster)
#cluster 2 & 3 shows the maximum number of Murder, Rape and Urbanpop
#cluster 4 shows Urbanpop crime is in second place
#cluster 1 compare to other cluster crime rate is less in all the criteria 



#An optimal value of 'k' is the value which gives us a converged set of clusters with minimum distortion. 
#Greater the distortion, worse will be the clusters formed.
#The distortion can be calculated in terms of  'withinss' from each of the clusters. Lesser the value of 'withinss' of a particular cluster,
#more densely populated it will be, thus minimum distortion.
kmeans.wss.k <- function(normalized_data, k){
  km = kmeans(normalized_data, k)
  return (km$tot.withinss)
}
kmeans.wss.k(normalized_data,4) #56.40317
kmeans.wss.k(normalized_data,5) #51.63029
kmeans.wss.k(normalized_data,10) #28.97545


library(animation)
cl<- kmeans.ani(normalized_data, 4)

#Kmeans clustering Algorithm:
 #  Let us understand the algorithm on which k-means clustering works:
 #  Step #1. If k=4, we select 4 random points and assume them to be cluster centers for the clusters to be created.
#Step #2. We take up a random data point from the space and find out its distance from all the 4 clusters centers. If  the data point is closest to the green cluster center, it is colored green and similarly all the points are categorised among the 4 clusters.
#Step #3. Now we calculate the centroid of all the green points and assign that point as the cluster center for that cluster.
#Similarly, we calculate centroids for all the 4 colored(clustered) points and assign the new centroids as the cluster centers.
#Step #4. Step-2 and step-3 are run iteratively, unless the cluster centers converge at a point and no longer move.
#Thus, we reach the converged clusters centers.



#Cluster Stability - K-means - Silhouette
library(ggplot2)
library(cluster)
library(factoextra)
km.3 <- eclust(kmcrime[,2:5], "kmeans", k = 4, nstart = 25, graph = TRUE)
fviz_silhouette(km.3)
#Average Silhouette width is 0.5
km.3 <- eclust(kmcrime[,2:5], "pam", k = 4, graph = TRUE)
fviz_silhouette(km.3)
#Average Silhouette width is 0.49














#kmeans.dis <- function(normalized_data, maxk){
#  + dis=(nrow(normalized_data)-1)*sum(apply(normalized_data,2,var))
#  + dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, normalized_data=normalized_data)
# + return(dis)
#   }
# maxk = 10
# dis <- kmeans.dis(normalized_data, 10);
# plot(1:maxk, dis, type='b', xlab="Number of Clusters",
 #      + ylab="Distortion",
  #     + col="blue")


