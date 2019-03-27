#Perform Principal component analysis and perform clustering using first 
#3 principal component scores (both heirarchial and k mean clustering(scree plot or elbow curve) and obtain 
#optimum number of clusters and check whether we have obtained same number of clusters with the original data 
#(class column we have ignored at the begining who shows it has 3 clusters)df

#Read File
setwd("C:\\Users\\Bharathyramakrishnan\\Documents\\R Prog\\ExcelR\\Excelr Data-\\Excelr Data\\Assignments\\PCA")
wine <- read.csv("wine.csv")
summary(wine)
View(wine)
# Scatter Plot & Correlations
library(psych)
pairs.panels(wine)
table(wine$Type)
pairs(wine[,-1], col = (wine$Type), upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("Type1","Type2","Type3"), pch = 16, col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5)
#Among other things, we observe correlations between variables (e.g.phenols and flavonoids), and occasionally the two-dimensional separation of the 3 Types.


library(corrplot)
winecorr <- cor(wine)
corrplot(winecorr,method="number")
#Correlation Table indicates that Phenols and Flavanoids (0.86) 
#and there is a negative correlation between flavanoids and Type(-0.85)

#Finding PCA Value using Princomp fn
pcaObj<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
pcaObj$center
str(pcaObj)
summary(pcaObj)
loadings(pcaObj)
plot(pcaObj) # showing the imporatance of Principal Component Comp1 is having Highest variance
biplot(pcaObj)

plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
pcaObj$loadings
pcaObj$scores[,1:3] #Top 3 PCA Scores which represents the whole data

# Considering top 3 principal component scores and perfoeming hclust and K-means
pcadata<-pcaObj$scores[,1:3]
View(pcadata)

# Hierarchial Clustering
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-pcadata

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean")

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete")
plot(fit1)

rect.hclust(fit1, k=5, border="red")

groups<-cutree(fit1,5) 

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,pcadata) # binding column wise with orginal data
View(final1)
View(aggregate(final1,by=list(membership_1),FUN=mean))

#K-Means
View(pcadata)

normalized_data<-scale(pcadata)

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:5) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

library(plyr)
library(factoextra)
fit <- eclust(normalized_data, "kmeans", k = 5, nstart = 25, graph = FALSE) # 5 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")

final2<- data.frame(fit$cluster,pcadata) # append cluster membership
View(final2)
aggregate(pcadata, by=list(fit$cluster), FUN=mean)

table(fit$cluster)

#Cbind the PCA and memebrship data 
pcadata<-cbind(wine,final2)
View(pcadata)

#Applying "prcomp" fn
WinePCA <-prcomp(scale(wine[,-1]))
plot(WinePCA$x[,1:2], col = wine$Type)
summary(WinePCA) # proportaion of Variance and sd is high in PC1, PC2, and Pc3 
biplot(WinePCA)
WinePCA$rotation #loadings
WinePCA$x #Scale

