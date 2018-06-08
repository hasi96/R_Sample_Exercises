library(tidyverse)
install.packages("randomForest")
library(randomForest)
train <- read_csv("train.csv")
test<- read_csv("test.csv")
data <- rbind.data.frame(train[,-1], test)
firstHorizontalPix <- vector()
lastHorizontalPix <- vector()
firstVerticalPix <- vector()
lastVerticalPix <- vector()
densityUpperLeft <- vector()
densityUpperRight <- vector()
densityBottomLeft <- vector()
densityBottomRight <- vector()
numberHorizontalLines <- vector()
numberVerticalLines <- vector()
symmetryHorizontal <- vector()
symmetryVertical <- vector()
densityUpper <- vector()
densityLower<- vector()

t0 <- Sys.time()

for (i in 1:nrow(data)){
  m = matrix(unlist(data[i,]),nrow = 28,byrow = T)
  csum<-0
  rsum <-0
  csum <- colSums(m)
  rsum <- rowSums(m)
  firstHorizontalPix <- c(firstHorizontalPix, min(which(rsum > 0)))
  lastHorizontalPix <- c(lastHorizontalPix, max(which(rsum > 0)))
  firstVerticalPix <- c(firstVerticalPix, min(which(csum > 0)))
  lastVerticalPix <- c(lastVerticalPix, max(which(csum > 0)))
  densityUpperLeft <- c(densityUpperLeft,sum(m[1:14,1:14])/sum(m))
  densityUpperRight <- c(densityUpperRight,sum(m[1:14,15:28])/sum(m))
  densityBottomLeft <- c(densityBottomLeft,sum(m[15:28,1:14])/sum(m))
  densityBottomRight <- c(densityBottomRight,sum(m[15:28,15:28])/sum(m))
  densityUpper <- c(densityUpper,sum(m[1:14,1:28])/sum(m))
  densityLower <- c(densityLower,sum(m[15:28,1:28])/sum(m))
  numberHorizontalLines <- c(numberHorizontalLines, sum(rsum >0))
  numberVerticalLines <- c(numberVerticalLines, sum(csum >0))
  symmetryHorizontal <- c(symmetryHorizontal, sum((m[,28:15] > 0) * (m[,1:14] > 0)))
  symmetryVertical <- c(symmetryVertical, sum((m[28:15,] > 0) * (m[1:14,] > 0)))
}
engFeatures <- cbind.data.frame(firstHorizontalPix, lastHorizontalPix,
                                firstVerticalPix, lastVerticalPix,
                                densityUpperLeft, densityUpperRight,
                                densityBottomLeft, densityBottomRight,
                                densityUpper,densityLower,
                                numberHorizontalLines, numberVerticalLines,
                                symmetryHorizontal, symmetryVertical)
labels <- train[1:nrow(train),1:1]
trainFeatures <- engFeatures[1:nrow(train),]
testFeatures <- engFeatures[nrow(train)+1:nrow(engFeatures),]

trainFeatures <-  cbind.data.frame(trainFeatures,labels)
rf <- randomForest(factor(label) ~ firstHorizontalPix + firstVerticalPix + lastHorizontalPix+ lastVerticalPix + densityBottomLeft + densityUpperLeft + densityUpper + densityLower+ densityUpperRight + densityBottomRight + numberHorizontalLines+ numberVerticalLines+symmetryHorizontal + symmetryVertical ,data =trainFeatures)
MeanDecreaseGini = rf$importance[,"MeanDecreaseGini"]
print(which(MeanDecreaseGini>2500))
install.packages("BBmisc")
library(BBmisc)
train_norm = normalize(trainFeatures, method = "standardize", range = c(0, 1), margin = 2, on.constant = "quiet")

install.packages("class")
library(class)
prediction = predict(rf,testFeatures,"response")
submit <- data.frame(ImageId = seq(1,nrow(testFeatures)),
                     +                      Label = prediction)
write.csv(submit, file = "sample_submission.csv", row.names=F)
