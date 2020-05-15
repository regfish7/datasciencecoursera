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


##### Air Pollution Case Study ####
# Are air pollution levels lower now than they were before? Lower in 2012 than in 1999?
#http://goo.gl/soQZHM (data found here) or https://aqs.epa.gov/aqsweb/airdata/download_files.html

pm0 <- read.table('data/pm25_data/RD_501_88101_1999-0.txt', comment.char= "#", header = FALSE, sep = "|", na.strings = "" )
dim(pm0)
head(pm0)
cnames <- readLines('data/pm25_data/RD_501_88101_1999-0.txt',1)
cnames
cnames <- strsplit(cnames, "|", fixed = TRUE) #makes it into a list, so you only need the first element
cnames
names(pm0) <- cnames[[1]]
head(pm0)
names(pm0) <- make.names(cnames[[1]]) #replaces spaces with dots so a valid name
head(pm0)

x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0)) #about 11% of the data are missing

#read in 2012 data
pm1 <- read.table('data/pm25_data/RD_501_88101_2012-0.txt', comment.char= "#", header = FALSE, sep = "|", na.strings = "" )
dim(pm1) #rows* columns * 8 bytes to get amount of memory needed to store
names(pm1)<- make.names(cnames[[1]])
x1 <- pm1$Sample.Value
str(x1)

#Quick comparison of datasets
summary(x1)
summary(x0)
mean(is.na(x1)) #only about 5% missing data
boxplot(x0,x1)
boxplot(log10(x0), log10(x1)) #evens out boxplot a little (error because negative values in original set)
summary(x1)
negative <- x1 <0
str(negative)
mean(negative, na.rm = TRUE) #about 2% of the values are negative
dates <- pm1$Date
str(dates) #in integer format, not useful
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, "month")
hist(dates[negative],"month")

#Explore values at one monitor (in NY)
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

head(site0)
site0 <- paste(site0[,1], site0[,2], sep = ".") #just create one variable code.siteID
site1 <- paste(site1[,1], site1[,2], sep = ".")
str(site0)
str(site1)
both <- intersect(site0,site1) #monitors in both datasets
both

pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
head(cnt0)

#split data frames by each monitor
sapply(split(cnt0, cnt0$county.site), nrow) #count number of observations in each data frame
sapply(split(cnt1, cnt1$county.site), nrow)

pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm1sub)
dim(pm0sub)

#plot pm2.5 data over time
dates1<- pm1sub$Date
x1sub <- pm1sub$Sample.Value
dates1 <- as.Date(as.character(dates1),"%Y%m%d")
plot(dates1, x1sub)

x0sub <- pm0sub$Sample.Value
dates0 <- as.Date(as.character(pm0sub$Date),"%Y%m%d")
plot(dates0, x0sub)

par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20)
abline(h=median(x0sub,na.rm=T))
plot(dates1, x1sub, pch = 20)
abline(h=median(x1sub,na.rm=T))

#need to put the two plots on the same range
rng <- range(x0sub, x1sub, na.rm=T)
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h=  median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h=  median(x1sub, na.rm = T))

#Explore change at the state level
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=T))
summary(mn1)
d0 <- data.frame(state=names(mn0), mean = mn0)
d1 <- data.frame(state=names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)
par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2], xlim= c(1998,2013)))
with(mrg, points(rep(2012, 52), mrg[,3]))
segments(rep(1999,52), mrg[,2], rep(2012,52),mrg[,3]) #draw lines between points of same year
