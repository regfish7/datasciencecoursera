## Lessson 1: Graphs ##

#Principles of Analytic Graphics
# See book "Beautiful Evidence" by Edward Tuffey
# 1. Show comparisons: evidence is always relative -> compared to what? 
# 2. Show casuality to make an explanation -> how do you believe the system is operating? 
# 3. Show multivariate data -> show as much data on one plot as you can.
# 4. Integrate the evidence you have -> use as many differnt modes of evidence as you can. 
# 5. Describe and document the evidence you present
# 6. Content is king -> how will I present this story?

# Exploratory Graphs
# Graphs that are for personal understanding. Made quickly, to get understanding of dataset. 

#1-D summaries
pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric","character","factor","numeric","numeric"))
head(pollution)
#Do any counties exceed the standard of 12 mu g/m^3 ? #note: fips is county locator
summary(pollution$pm25) #five number summary + mean
boxplot(pollution$pm25, col = "blue")

hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25) #shows all the data points underneath

boxplot(pollution$pm25, col = "blue")
abline(h=12) #gives horizontal line

hist(pollution$pm25, col = "green")
abline(v =12, lwd=2) #upper threshold line
abline(v = median(pollution$pm25),col = 'magenta',lwd = 4) #median of the data

#summarize a categorical variable
barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")

#2-D (+) summaries
boxplot(pm25~region, data = pollution, col = "red") #2 box plots over east and west


par(mfrow = c(2,1), mar = c(4,4,2,1)) #multiple histograms
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

#scatter plot
with(pollution, plot(latitude, pm25, col = region)) #col works as hue in python (second plot)
abline(h=12, lwd = 2, lty =2)

par(mfrow = c(1,2), mar = c(5,4,2,1)) #multiple plots
with(subset(pollution, region == "west"),plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"),plot(latitude, pm25, main = "East"))
