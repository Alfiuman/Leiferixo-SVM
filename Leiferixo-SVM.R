setwd("C:\\Users\\Home\\Documents\\Machine Learning\\Project")

library(e1071)

#########################################################
##Variables
testPercSize <- 0.4
seed <- 0
kernel <- 2
tuning <- 0
#########################################################

completeData <- as.matrix(read.csv("breast-cancer-wisconsin.data.txt"))
completeData <- completeData[,-1]
completeData[completeData == "?"] <- 100
class(completeData) <- "numeric"

set.seed(seed)
completeData <- completeData[sample(nrow(completeData)),]
trainData <- completeData[c(1:round(((1 - testPercSize) * nrow(completeData)))),]
testData <- completeData[c(((round((1 - testPercSize) * nrow(completeData))) + 1):nrow(completeData)),]

testDataX <- testData[,1:(ncol(testData) - 1)]
testDataY <- testData[,ncol(testData)]

correct <- 0
wrong <- 0
total <- nrow(testData)

if (kernel == 0)
{
  kern <- "linear"
} else if (kernel == 1)
{
  kern <- "polynomial"
} else if (kernel == 2)
{
  kern <- "radial"
} else 
{
  kern <- "sigmoid"
}

if (tuning == 0)
{
svmMod <- svm(trainData[,ncol(trainData)] ~ ., data = trainData, type = "C-classification", kernel = kern)
pred <- predict(svmMod, testData)
print(summary(svmMod))
} else
{
tunMod <- tune.svm(as.matrix(trainData[,1:(ncol(trainData) - 1)]), as.factor(trainData[,ncol(trainData)]), type = "C-classification", kernel = kern)
tunedSVM <- tunMod$best.model
pred <- predict(tunedSVM, testDataX)
print(summary(tunedSVM))
}

results <- testDataY

for(i in 1:length(testDataY))
{
  if (pred[i] == testDataY[i])
  {
    results[i] <- 1
    correct <- correct + 1
  }
  else
  {
    results[i] <- 0
    wrong <- wrong + 1
  }
}

correctPerc <- (correct / total) * 100
paste("The correct predictions are ", correctPerc, "%", sep ="")












