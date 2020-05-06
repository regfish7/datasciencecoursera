### Hierarchical Clustering ###

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