#### Lesson 1:  Hierarchical Clustering ####

#hierarchical clustering is an agglomerative, or bottom-up, approach.  

#Generate points
set.seed(1234)
par(mar = c(0,0,0,0))
x <-rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1),each = 4), sd = 0.2)
plot(x,y, col = "blue", pch =19, cex=2)
text(x+0.05, y+0.05, labels = as.character(1:12)) #label points

#Run algorithm to see how they get merged together
dataFrame <- data.frame(x=x, y=y)
distxy <- dist(dataFrame) #calculate distance between points (output is a lower triangular matrix of pairwise (Euclidean) distances)

#start with two points closest together, group and merge (points 5 and 6)
#Then find next to closest points (10, 11) and create a new point, cont.

hClustering <- hclust(distxy)
plot(hClustering) #creates dendogram (furthest down tree got clustered first); doesn't tell how many clusters
#cut tree at certain point (horizontal line) to determine how many clusters there are. 

#Prettier dendograms
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
    ## modifiction of plclust for plotting hclust objects *in colour*!
    ## Copyright Eva KF Chan 2009
    ## Arguments:
    ##    hclust:    hclust object
    ##    lab:        a character vector of labels of the leaves of the tree
    ##    lab.col:    colour for the labels; NA=default device foreground colour
    ##    hang:     as in hclust & plclust
    ## Side effect:
    ##    A display of hierarchical cluster with coloured leaf labels.
    y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
    y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
    y <- y[order(x)]; x <- x[order(x)]
    plot( hclust, labels=FALSE, hang=hang, ... )
    text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
          labels=lab[hclust$order], col=lab.col[hclust$order], 
          srt=90, adj=c(1,0.5), xpd=NA, ... )
}

myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4)) #labled by cluster label and colored
# gallery.r-enthusiasts.com/RGraphGallery.php?graph=79

set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)
# a very nice concise tutorial on creating heatmaps in R exists at http://sebastianraschka.com/Articles/heatmaps_in_r.html#clustering

### Lesson 2: K-Means Clustering & Dimension Reduction ####
#set up K-means visualization with 3 obvious clusters 
set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x,y, col = "blue", pch = 19, cex = 2)
text(x+0.05, y + 0.05, labels = as.character(1:12))

#The process visualized, from swirl#
points(cx, cy, col = c("red", "orange", "purple"), pch = 3, cex = 2, lwd = 2)
distTmp <- mdist(x,y, cx, cy) #distance between each point and each centroid
apply(distTmp, 2, which.min) #find min of each column
points(x, y, pch = 19, cex = 2, col = cols1[newClust]) #color the points based on which cluster they are in
newCx <- tapply(x, newClust, mean) #calculate new means, xcoords
newCy <- tapply(y, newClust, mean) #calculate new means, ycoords
points(newCx, newCy, col = cols1, pch = 8, cex = 2, lwd = 2) #plot new centroids as *
distTmp2 <- mdist(x,y,newCx,newCy) # calculate distances to reassign data points to new clusters
newClust2 <- newapply(distTmp2, 2, which.min) #find new cluster assginments for points
points(x,y,pch=19,cex=2,col=cols1[newClust2]) #recolor points
finalCx <- tapply(x,newClust2,mean) #find x coord of new centroid
finalCy <-tapply(y, newClust2,mean) #find y coord of new centroid
points(finalCx,finalCy,col=cols1, pch = 9, cex = 2, lwd = 2) #plot new centroids
#etc....
#end swirl visualization#

dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
kmeansObj$cluster #shows which points are in which cluster
#centers tells you the locations of the centroids
# From swirl: it's called kmeans and, although it has several parameters, we'll just mention four. These are x, 
# (the numeric matrix of data), centers, iter.max, and nstart. The second of these (centers) can be either a number
# of clusters or a set of initial centroids. The third, iter.max, specifies the maximum number of iterations to go 
# through, and nstart is the number of random starts you want to try if you specify centers as a number.

#visualize results
par(mar = rep(0.2, 4))
plot(x,y, col = kmeansObj$cluster, pch =19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3) #add the centroids


#heatmap visualization
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),] #take a different random sample of the data
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1,2), mar = c(2,4,0.1,0.1))
image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[,order(kmeansObj2$cluster)],yaxt="n") #reorganize to put clusters together

## PCA and SVD ##
set.seed(12345)
par(mar = rep(0.2,4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1]) #like imagesc in MATLAB

par(mar = rep(0.2,4))
heatmap(dataMatrix)

#add pattern to dataset
set.seed(678910)
for (i in 1:40){
    #flip a coin
    coinFlip <- rbinom(1,size = 1, prob = 0.5)
    #if coin is heads add a common pattern to that row
    if (coinFlip){
        dataMatrix[i, ]<- dataMatrix[i, ] + rep(c(0,3), each =5 )
    }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

par(mar = rep(0.2,4)) 
heatmap(dataMatrix) #clearer clustering in the columns

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,] #reorder according to heirarchical clustering of rows
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1,xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab="Column Mean", pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19) #clearly indicates shift in means
plot(svd1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19) #raw singular values
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19) #proportion of variance explained (normalized)

pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[,1], svd1$v[,1],pch =19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1") #pca and svd basically doing same thing
abline(c(0,1))

#simple example - 1 pattern
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1), each = 5)}
svd1 <- svd(constantMatrix)
par(mfrow = c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19) #100% explained by first component

#add patterns to dataset (across rows and columns)
set.seed(678910)
for (i in 1:40){
    #flip a coin
    coinFlip1 <- rbinom(1,size = 1, prob = 0.5)
    coinFlip2 <- rbinom(1,size = 1, prob = 0.5)
    #if coin is heads add a common pattern to that row
    if (coinFlip1){
        dataMatrix[i, ]<- dataMatrix[i, ] + rep(c(0,5), each =5 )
    }
    if (coinFlip2){
        dataMatrix[i, ]<- dataMatrix[i, ] + rep(c(0,5), each =5 )
    }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]

svd2 <- svd(scale(dataMatrixOrdered)) #scale subtracts the col mean from each entry and then divides by the col std
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rep(c(0,1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1") #the truth
plot(rep(c(0,1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd2$v[,1], pch = 19, xlab = "Column", ylab = "First right singular vector") #principle components
plot(svd2$v[,2], pch = 19, xlab = "Column", ylab = "Second right singular vector")

par(mfrow=c(1,2))
plot(svd2$d, xlab = "Column", ylab = "Singular Value", pch = 19)
plot(svd2$d^2/sum(svd2$d^2), xlab = "Column", ylab = "Percent of variance explained", pch=19)

# Missing values
dataMatrix2 <- dataMatrixOrdered
# Randomly insert some missing data
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2)) #doesn't work!

library(impute) 
dataMatrix2 <- impute.knn(dataMatrix2)$data #imputes by k nearest neighbors
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch =19) #imputation didn't affect the SVD greatly

#Image example
par(mfrow=c(1,1))
load("data/face.rda")
image(t(faceData)[,nrow(faceData):1])

svd1 <- svd(scale(faceData))
plot(svd2$d^2/sum(svd2$d^2), xlab = "Singular Vector", ylab = "Percent of variance explained", pch=19)

#create approximation to face (data compression)
#Note that %*% is matrix multiplication

#Here svd1$d[1] is a constant
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]

#In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5]) #approx with first 5 sv's
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]) #approx with first 10 sv's

par(mar = c(1, 1, 1, 1),mfrow = c(1,4))
image(t(approx1)[, nrow(approx1):1], main = "(a)")
image(t(approx5)[, nrow(approx5):1], main = "(b)")
image(t(approx10)[, nrow(approx10):1], main = "(c)")
image(t(faceData)[, nrow(faceData):1], main = "(d)")

#from swirl: PCA command is prcomp(scale(mat))
# Notice that the principal components of the scaled matrix, shown in the Rotation component of the prcomp output, ARE the
# columns of V, the right singular values. Thus, PCA of a scaled matrix yields the V matrix (right singular vectors) of the same
# scaled matrix.
