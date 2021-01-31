library(rpart)
library(rpart.plot)
library(rattle)
library(readr)
library(car)
library(caret)
library(tidyverse)
library(gridExtra)
library(GGally)
Data<-read.csv("C:/Users/santu/OneDrive/Desktop/spofity1.csv")
dim(Data)
str(Data)
summary(Data)
set.seed(1234)
plot(Data)
plot(Data$skipped)
plot(Data$duration)
table(Data$skipped)
# runif function returns a uniform distribution which can be further conditionally split into 75-25 ratio
Data[, 'train'] <- ifelse(runif(nrow(Data)) < 0.75, 1, 0)
trainSet <- Data[Data$train == 1,]
testSet <- Data[Data$train == 0, ]
nrow(trainSet)
ncol(trainSet)
dim(trainSet)
dim(testSet)
traincolnum <- grep('train', names(trainSet))
trainSet <- trainSet[, -traincolnum]
testSet <- testSet[, -traincolnum]
treefit <- rpart(skipped~.,data=trainSet,method = 'class')
print(treefit)
rpart.plot(treefit, box.col=c("red", "green"))
#prediction
result<-predict(treefit,newdata = Data,type="class")
result
### Deployment 
install.packages("shiny")
