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

########### Lesson 2: ggplot2 ##################
# gg = "Grammar of Graphics"
library(ggplot2)
str(mpg)
qplot(displ,hwy,data=mpg) #'hello, world!' plot (scatterplot)
qplot(displ,hwy,data=mpg, color = drv) #seperate colors by drive variable
qplot(displ, hwy, data= mpg, geom = c("point","smooth")) # add a statistic (trend line)/95% confidence interval for line
qplot(hwy,data=mpg,fill=drv) #histogram

#facets (panel plots for different subsets)
qplot(displ,hwy,data=mpg,facets=.~drv) #facets = rows ~ columns
qplot(hwy,data=mpg,facets=drv~.,binwidth=2)

#Example without data (MAACS data)
qplot(logpm25, NocturnalSympt, data = maacs, facets = .~bmicat, geom = c("point", "smooth"), method = "lm")
#Same plot, built up using ggplot
g <- ggplot(maacs, aes(logpm25, NocturnalSympt)) #no plot yet! No layers
p <- g + geom_point() # if you print(p) you get a picture on the screen
g + geom_point()+geom_smooth() #add points, then line with confidence band
g + geom_point() + geom_smooth(method = "lm") #linear regression line instead of "loess" smoother as previously
g + geom_point() + facet_grid(.~bmicat) + geom_smooth(method = "lm") #add facets (two plots) # of plots = # levels in factor
#order doesn't matter

g + geom_point(color = "steelblue", size = 4, alpha = 1/2) #color is a constant
g + geom_point(aes(color = bmicat), size = 4, alpha = 1/2) #color is not constant so needs to be wrapped in aesthetics function (color becomes data variable)
g + geom_point(aes(color = bmicat)) + labs(title = "MAACS Cohort") + labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")
# Note that in the above the x-axis label is "log PM_2.5" (subscript)
g + geom_point(aes(color = bmicat), size = 2, alpha =1/2) +
    geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE) #change smoother; se =FALSE turns off confidence interval
g + geom_point(aes(color = bmicat)) + theme_bw(base_family = "Times") #changes background and font

# Part 5
testdat <- data.frame(x=1:100, y=rnorm(100)) #some random data 
testdat[50,2] <- 100 #with one outlier
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3)) #standard plot zoomed in to ignore outlier
g <- ggplot(testdat, aes(x=x, y=y)) #equivalent in ggplot
g + geom_line() #shows outlier
g+geom_line() + ylim(-3,3) #misses outlier in the middle of the plot (only includes points whose y value is the specified range)
g + geom_line() + coord_cartesian(ylim = c(-3, 3)) #outlier included (though goes off screen)

#Back to the MAACS data

#Calculate deciles of the data
cutpoints <- quantile(maacs$logno2_new, seq(0,1,length=4), na.rm = TRUE)

#Cut the data at the deciles and create a new factor variable
maacs$no2dec <- cut(maacs$logno2_new, cutpoints) 

#See levels of newly created factor variable
levels(maacs$no2dec) #three levels

#Setup ggplot with data frame
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))

#Add layers
g + geom_point(alpha = 1/3) #add points and change transparency
  + facet_wrap(bmicat ~ no2dec, nrow = 2, ncol = 4) #make panels
  + geom_smooth(method = "lm", se = FALSE, col = "steelblue") #add smoother (no confidence interval)
  + theme_bw(base_family= "Avenir", base_size = 10) #change theme and font
  + labs(x = expression("log "* PM[2.5])) #add labels
  + labs(y = "Nocturnal Symptoms")
  + labs(title = "MAACS Cohort")

#FROM SWIRL:
#  g + geom_point() + facet_grid(drv~cyl, margins = TRUE)

# A 4 by 5 plot, huh? The margins argument tells ggplot to display the marginal totals over each row and column, so
# instead of seeing 3 rows (the number of drv factors) and 4 columns (the number of cyl factors) we see a 4 by 5
# display. Note that the panel in position (4,5) is a tiny version of the scatterplot of the entire dataset.