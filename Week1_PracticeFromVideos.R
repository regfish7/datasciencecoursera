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


#### LESSON 2: Plotting ####
##Plotting systems in R
# Base Plotting System
library(datasets)
data(cars)
with(cars,plot(speed,dist))

#Lattice System
library(lattice)
state <- data.frame(state.x77,region = state.region)
xyplot(Life.Exp~Income | region, data = state, layout = c(4,1))

##ggplot2
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

##Base Plotting System
?par #documentation for graphical parameters
library(datasets)
hist(airquality$Ozone)

with(airquality, plot(Wind, Ozone)) #scatterplot

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone~Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

# pch = plotting symbol (number or character)
# lty = line type
# lwd = line width
# col = plotting color (number, string, hex code, colors() function gives vector of colors by name)
# xlab = x-axis label
# Global parameters by par():
# las = orientation of axis labels
# bg = background color
# mar = margin size (bottom, left, top, right)
# oma = outer margin size (default is 0 for all sides)
# mfrow = number of plots per row, column (plots filled row-wise)
# mfcol = number of plots per row, column (plots filled col-wise)
# ylab = y-axis label

#plot = scatterplot
#lines = adds lines to plot
#points = add points to plot
#text = add text labels to plot
#title = add anotations to axis labels, titles, etc
#mtext  = add aribitrary text to margins of plot
#axis = adding axis ticks/labels

with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue")) #highlight data in month of May

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n")) #don't plot yet
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue")) #highlight data in month of May
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May","Other Months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch=20))
model <- lm(Ozone~Wind, airquality)
abline(model,lwd=2)

par(mfrow=c(1,2))
with(airquality, {
    plot(Wind, Ozone, main = "Ozone and Wind")
    plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow=c(1,3), mar = c(4,4,2,1), oma=c(0,0,2,0)) #make outer margin at top bigger so can include overall title
with(airquality, {
    plot(Wind, Ozone, main = "Ozone and Wind")
    plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
    plot(Temp, Ozone, main = "Ozone and Temperature")
    mtext("Ozone and Weather in New York City", outer = TRUE) #overall title in the margin
})

###Base Plotting Demonstration
x <- rnorm(100)
hist(x)
par(mfrow=c(1,1)) #to clear previous plotting parameters
y <-rnorm(100)
plot(x,y)
z <- rnorm(100)
plot(x,z)
plot(x,y)
par(mar = c(2,2,2,2)) #change margins
plot(x,y)
par(mar = c(4,4,2,2))
plot(x,y)
plot(x,y,pch=20)#filled in dot
plot(x,y,pch=19)
plot(x,y,pch=2) #triangles
example(points) #goes through demo (third screen gives visuals of symbols)

x<- rnorm(100)
y <- rnorm(100)
plot(x,y, pch=20)
title("Scatterplot")
text(-2,-2, "label")
legend("topleft",legend = "Data", pch = 20)
fit <- lm(y~x)
abline(fit)
abline(fit,lwd=3) #plots new line on top
abline(fit, lwd=3, col ="blue")
plot(x,y, xlab = "Weight" , ylab = "Height", main = "Scatterplot", pch = 20)
legend("topright", legend = "Data", pch = 20)
fit <- lm(y~x)
abline(fit, lwd=3, col = "red")

z <- rpois(100,2)
par(mfrow=c(2,1))
plot(x,y, pch = 20)
plot(x,z, pch = 19)
par(mar = c(2,2,1,1))
plot(x,y, pch = 20)
plot(x,z, pch = 19)
par(mfrow = c(1,2))
plot(x,y, pch = 20)
plot(x,z, pch = 19)
par(mar = c(4,4,2,2))
par(mfrow=c(2,2))
plot(x,y)
plot(x,z)
plot(z,x)
plot(y,x)
part(mfcol=c(2,2))
plot(x,y)
plot(x,z)
plot(z,x)
plot(y,x)

par(mfrow = c(1,1))
x <- rnorm(100)
y <- x+ rnorm(100)
g <- gl(2,50)
g <- gl(2,50, labels = c("Male", "Female"))
str(g)
plot(x,y)
plot(x,y, type = "n") #everything missing but data
points(x[g == "Male"], y[g=="Male"], col = "green")
points(x[g=="Female"], y[g=="Female"], col = "blue", pch = 19)
#Use dev.off() or plot.new() to reset to the defaults.


### Graphics Devices in R
?Devices
library(datasets)
with(faithful, plot(eruptions, waiting)) #plot appears on screen device
title(main = "Old Faithful Geyser data")

pdf(file = "myplot.pdf") #open pdf device; created in working directory
with(faithful, plot(eruptions, waiting)) #plot will not appear on screen
title(main = "Old Faithful Geyser data")
dev.off() #close the PDF file device

dev.cur() #currently active graphics device
#dev.set(<integer>) #switch to a different graphics device

with(faithful, plot(eruptions, waiting)) #plot will not appear on screen
title(main = "Old Faithful Geyser data")
dev.copy(png,file = "geyserplot.png") #copy my plot to a PNG file
dev.off() #close the PNG device!
