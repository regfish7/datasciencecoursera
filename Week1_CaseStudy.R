# Question: Can I use quantitative characteristics of the emails themselves to classify them as spam or ham? 
# Where the dataset came from and how it was processed: http://search.r-project.org/library/kernlab/html/spam.html

library(kernlab)
data(spam)
str(spam[,1:5])

#Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator) #show number of testing and training samples
trainSpam = spam[trainIndicator == 1,]
testSpam = spam[trainIndicator == 0,]

names(trainSpam)
head(trainSpam) #freq at which they occur in a given email
table(trainSpam$type)

## Exploratory Data Analysis
plot(trainSpam$capitalAve ~trainSpam$type) #captial letters
plot(log10(trainSpam$capitalAve+1)~trainSpam$type) #log better because original data skewed (add 1 to avoid 0 in log)
plot(log10(trainSpam[,1:4]+1))

hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

#transform in predictor space then do clustering 
hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

##Statistical prediction/modeling (here, fit logistic regression)
trainSpam$numType = as.numeric(trainSpam$type)-1
costFunction = function(x,y) sum(x != (y > 0.5))
cvError = rep(NA, 55) #cross-validated error rate
library(boot)
for(i in 1:55){
    lmFormula = reformulate(names(trainSpam)[i], response = "numType")
    glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
    cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

# Which predictor has the minimum cross-validated error? 
names(trainSpam)[which.min(cvError)] #charDollar = # dollar signs in the email

#Use the best model from the group
predictionModel = glm(numType~charDollar, family = "binomial", data = trainSpam)

## Get predictions on the test set
predictionTest = predict(predictionModel,testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

#Classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted >0.5]= 'spam'

#classification table
table(predictedSpam, testSpam$type)

#Error rate
(61+458)/(1346+458+61+449) #about 22%


