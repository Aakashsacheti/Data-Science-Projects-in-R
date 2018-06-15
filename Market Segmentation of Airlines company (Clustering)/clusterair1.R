
Path<-setwd("C:/Users/aakas/OneDrive/IVY/IVY- R Arpendu/data")
getwd()


airlines<-read.csv("AirlinesCluster.csv")


#Understand the data type and summary of each coloumn
head(airlines)
str(airlines)
summary(airlines)
View(airlines)



#Checking missing values
as.data.frame(colSums(is.na(airlines)))

#installing required library
install.packages("caret", dependencies = TRUE)

#Normalizing the Data for clustering 
pre<- caret::preProcess(airlines)
View(pre)
normair<-predict(pre,airlines)
View(normair)
summary(normair)


#computing distance bw data points
interval<-dist(normair, method = "euclidean")

#Hierarchical Clustering
Cluster1<-hclust(interval, method = "ward")
plot(Cluster1)


#Splitting data in clusters
cluster2<-cutree(Cluster1, k = 5)
#getting number of datapoints in each cluster
table(cluster2)

#average values of the cluster groups
mean_bal<-tapply(airlines$Balance, cluster2, mean)
mean_Qmiles<-tapply(airlines$QualMiles, cluster2, mean)
mean_bmiles<-tapply(airlines$BonusMiles, cluster2, mean)
mean_btrans<-tapply(airlines$BonusTrans, cluster2, mean)
mean_fmiles<-tapply(airlines$FlightMiles, cluster2, mean)
mean_ftrans<-tapply(airlines$FlightTrans, cluster2, mean)
mean_dy_sncEnroll<-tapply(airlines$DaysSinceEnroll, cluster2, mean)

cluster_avg <- cbind(mean_bal,mean_Qmiles,mean_bmiles, mean_btrans,
                     mean_fmiles, mean_ftrans, mean_dy_sncEnroll )

View(cluster_avg)
write.csv(cluster_avg,"clusterAir_avg.csv" )

#k-Means Clustersing
set.seed(01)
kmsa<- kmeans(normair, 5, iter.max = 1000)
View(kmsa)


# Clusters with more than 1000 obs
cbal<-tapply(airlines$Balance, kmsa$cluster, mean)
cqmiles<-tapply(airlines$QualMiles, kmsa$cluster, mean)
cbmiles<-tapply(airlines$BonusMiles, kmsa$cluster, mean)
cbtrans<-tapply(airlines$BonusTrans, kmsa$cluster, mean)
cfmiles<-tapply(airlines$FlightMiles, kmsa$cluster, mean)
cftrans<-tapply(airlines$FlightTrans, kmsa$cluster, mean)
cdyenr<-tapply(airlines$DaysSinceEnroll, kmsa$cluster, mean)

kmeans_cluster<- cbind(cbal, cqmiles, cbmiles, cbtrans, cfmiles,
                       cftrans, cdyenr)

View(kmeans_cluster)
write.csv(kmeans_cluster, "kmeans_air.csv")
      

table(kmsa$cluster)
kmsa$centers
