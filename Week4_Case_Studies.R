#### Samsung Galaxy Phone Data (Clustering Case) ####
load("data/samsungData.rda")
names(samsungData)
table(samsungData$activity)

par(mfrow=c(1,2), mar=c(5,4,1,1))
samsungData <- transform(samsungData, activity = factor(activity)) #turn activity into factor variable
sub1 <- subset(samsungData, subject ==1)
plot(sub1[,1], col = sub1$activity, ylab = names(sub1)[1]) #mean body acceleration in x direction
plot(sub1[,2], col = sub1$activity, ylab = names(sub1)[2]) #mean body acceleration in y direction (colorcoded by activity)
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity),pch=1)

#Clustering based just on average acceleration
par(mfrow=c(1,1))
source("myplclust.R") #to visualize (created by R. Peng)
distanceMatrix <- dist(sub1[,1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

#Max acceleration for first subject
par(mfrow = c(1,2))
plot(sub1[,10], pch = 19, col = sub1$activity, ylab = names(sub1)[10]) #max body acceleration in x direction
plot(sub1[,11], pch = 19, col = sub1$activity, ylab = names(sub1)[11]) #max body acceleration in y direction (colorcoded by activity)

#clustering based on max acceleration
par(mfrow=c(1,1))
distanceMatrix <- dist(sub1[,10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

#SVD
svd1 = svd(scale(sub1[,-c(562, 563)])) #remove last two columns: subject and activity identifier (not interesting)
par(mfrow = c(1,2))
plot(svd1$u[,1], col = sub1$activity, pch = 19) #first singular vector: separates moving from not moving
plot(svd1$u[,2], col = sub1$activity, pch = 19) #second singular vector: separates magenta from other clusters

#find max contributer and cluster based on that 
par(mfrow = c(1,1))
plot(svd1$v[,2],pch = 19)
maxContrib <- which.max(svd1$v[,2])
distanceMatrix <- dist(sub1[,c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col = unclass(sub1$activity))
names(samsungData)[maxContrib] #the mean body acc in freq domain in z direction

#K-means clustering
kClust <- kmeans(sub1[,-c(562, 563)], centers=6, nstart = 100) #takes optimal from 100 different starts
table(kClust$cluster, sub1$activity)

plot(kClust$center[1,1:10], pch = 19, ylab = "Cluster Center", xlab = "") #cluster 1
plot(kClust$center[4,1:10], pch = 19, ylab = "Cluster Center", xlab = "") #cluster 2
