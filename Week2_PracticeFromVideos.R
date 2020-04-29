##### Lesson 1: Lattice Plotting System ################
#high dimensional data and making many plots as well. 

xyplot(y~x|f*g, data) #f, g categorical variables you condtion on (optional)

library(lattice)
library(datasets)
xyplot(Ozone~Wind, data = airquality)

airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone~Wind | Month, data = airquality, layout = c(5,1))

P <- xyplot(Ozone~Wind, data = airquality) #nothing happens! (saves as trellis object)
print(P) #now it plots

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x+f -f*x + rnorm(100, sd = 0.5) #"For its first 50 values y is a function of x, for the last 50 values, y is random" (from swirl)
f <- factor(f, labels = c("Group 1", "Group 2")) #"distinguishes between the first and last 50 elements of the two vectors" (from swirl)
xyplot(y~x | f, layout = c(2,1)) #plot with 2 panels

#Custom panel function
xyplot(y~x | f, panel = function(x,y,...){
    panel.xyplot(x,y,...) # First, call the default panel function of 'xyplot'
    panel.abline(h=median(y), lty =2) # Add a horizontal line at the median
})

xyplot(y~x | f, panel = function(x,y,...){
    panel.xyplot(x,y,...) # First, call the default panel function of 'xyplot'
    panel.lmline(x,y, col =2) # Overlay a simple linear regression line
})

#From swirl: xyplot(price~carat | color*cut, data = diamonds, strip = FALSE, pch =20, xlab = myxlab, ylab = myylab, main = mymain)
# strip = FALSE gets rid of the condition labels

#colorRamp, takes a palette of colors (the arguments) and returns a function that takes values between 0 and 1 as
# arguments. The 0 and 1 correspond to the extremes of the color palette. Arguments between 0 and 1 return blends of these extremes.

#We'll turn now to colorRampPalette, a function similar to colorRamp. It also takes a palette of colors and returns a function.
# This function, however, takes integer arguments (instead of numbers between 0 and 1) and returns a vector of colors each of which
# is a blend of colors of the original palette.

#The argument you pass to the returned function specifies the number of colors you want returned. Each element of the returned
# vector is a 24 bit number, represented as 6 hexadecimal characters, which range from 0 to F. This set of 6 hex characters
# represents the intensities of red, green, and blue, 2 characters for each color.

#These colorBrewer palettes (provides color palettes for sequential, categorical, and diverging data) can be used in conjunction with the colorRamp() and colorRampPalette() functions. You would use colors
# from a colorBrewer palette as your base palette,i.e., as arguments to colorRamp or colorRampPalette which would interpolate them
# to create new colors.