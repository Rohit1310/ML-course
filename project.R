install.packages("dplyr")
install.packages("rattle")
library("dplyr")
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(corrplot)
library(e1071)
setwd("~/pml")
set.seed(34213)
--------------------------------------------------
## Data loading
trainDt <- read.csv("pml-training.csv")
testDt <- read.csv("pml-testing.csv")
#namesDt <- names(trainDt) 
trainDt <- select(trainDt, -c(1:5))
testDt <- select(testDt, -c(1:5))
---------------------------------------------------
# Partitioning
inTrainSub <- createDataPartition(y = trainDt$classe, p = 0.7, list = FALSE)
subTrain <- trainDt[inTrainSub,]
subTest <- trainDt[-inTrainSub,]
---------------------------------------------------------
# Data Cleaning
NZV <- nearZeroVar(subTrain)
TrainSet <- subTrain[, -NZV]
TestSet  <- subTest[, -NZV]
AllNA    <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet <- TrainSet[, AllNA==FALSE]
TestSet <- TestSet[, AllNA==FALSE]
#trainingset<-trainDt[,colSums(is.na(TrainSet)) == 0]
#testingset <-testDt[,colSums(is.na(TrainSet)) == 0]
----------------------------------------------------------
## Random Forest
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modelFit_rf <- train(
                      classe ~ ., 
                      data = TrainSet,
                      method = "rf", 
                      trControl = controlRF
                    )
modelFit_rf$finalModel
pred_rf <- predict(modelFit_rf, TestSet)
confMatRandForest <- confusionMatrix(pred_rf, TestSet$classe)
confMatRandForest
-------------------------------------------------------------
## Decision Tree
modelFit_dt <- rpart(classe ~ ., data=TrainSet, method="class")
modelFit_dt$finalModel
pred_dt <- predict(modelFit_dt, TestSet)
rpart.plot(modelFit_dt, type = 3, fallen.leaves = FALSE)
prp(modelFit_dt,fallen.leaves = FALSE,varlen = 5)
-----------------------------------------------------------------------
## Naive Bayes
NVbayes <- naiveBayes(classe ~ .,data = TrainSet)
pred_nv <- predict(NVbayes,TestSet)
table(pred_nv,TestSet$classe)
confMatNVB <- confusionMatrix(pred_nv, TestSet$classe)
# conclusion: It is not giving a significate result as all the features are continous.
# ---------------------------------------------------
## SVM
modelFit_svm <- svm(classe ~ ., data = TrainSet)
pred_svm <- predict(modelFit_svm,TestSet)
confMatSVM <- confusionMatrix(pred_svm, TestSet$classe)

#------------------------------------------------------
##plots
tsvm <- table(pred_svm,TestSet$classe)
tnv <- table(pred_nv,TestSet$classe)
trf <- table(pred_rf,TestSet$classe)
plot(tsvm, col = "yellow")
plot(trf, col = "red")
plot(tnv, col = "blue")