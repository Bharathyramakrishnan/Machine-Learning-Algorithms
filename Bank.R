# FPR
fpr = performance(pred, "fpr")@y.values[[1]]
cutoff = performance(pred, "fpr")@x.values[[1]]

# FNR
fnr = performance(pred,"fnr")@y.values[[1]]

#plot the FPR and FNR versus threshold values
matplot(cutoff, cbind(fpr,fnr), type="l",lwd=2, xlab="Threshold",ylab="Error Rate")
legend(0.4, 1, legend=c("False Positive Rate","False Negative Rate"),
       col=c(1,2), lty=c(1,2))

# calculate the euclidean distance between (fpr,fnr) and (0,0)
rate = as.data.frame(cbind(Cutoff=cutoff, FPR=fpr, FNR=fnr))
rate$distance = sqrt((rate[,2])^2+(rate[,3])^2)

# select the probability threshold with the smallest euclidean distance
index = which.min(rate$distance)
best = rate$Cutoff[index]
best
abline(v=best, col=3, lty=3, lwd=3)




#calculate subscription probabilities again with threshold value
bankfit1 = Bankdat %>%
  mutate(predy=as.factor(ifelse(prob<=0.099, "No", "Yes")))

# confusion matrix
table(pred=bankfit1$predy, true=Bankdat$y)

#Decision Trees
#bank <- Bankdat[,-7] 

#library(ISLR)
#library(tree)

#tree.bank = tree(y~.-poutcome, data=Bankdat)
#summary(tree.bank)

#plot(tree.bank)
#text(tree.bank, pretty=0, cex=1, col="purple")
#title("Classification Tree")

#Since Job is category variable so creating dummy 
#library(dummies)
#Job_dummy<-cbind(Bankdat,dummy(Bankdat$job,sep = "-"))
#View(Job_dummy)
#Job_dummy1<- as.data.frame(Job_dummy)
#class(Job_dummy1)
#View(Job_dummy1)
#colnames(Job_dummy1)

# Similary dummy for variable marital
#mar_dummy<-cbind(Job_dummy1,dummy(Bankdat$marital,sep = "-"))
#View(mar_dummy)
#mar_dummy1<- as.data.frame(mar_dummy)
#View(mar_dummy1)
#colnames(mar_dummy1)
#JobMar_Bank <- rbind.data.frame(Job_dummy1,mar_dummy1,sort = TRUE)




