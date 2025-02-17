#install packages

if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}

if (!require("ROSE")) {
  install.packages("ROSE", repos="http://cran.rstudio.com/") 
  library("ROSE")
}

if (!require("car")) {
  install.packages("car", repos="http://cran.rstudio.com/") 
  library("car")
}


if (!require("plyr")) {
  install.packages("plyr", repos="http://cran.rstudio.com/") 
  library("plyr")
}

if (!require("caret")) {
  install.packages("caret", repos="http://cran.rstudio.com/") 
  library("caret")
}

if (!require("party")) {
  install.packages("party", repos="http://cran.rstudio.com/") 
  library("party")
}

if (!require("RWeka")) {
  install.packages("RWeka", repos="http://cran.rstudio.com/") 
  library("RWeka")
}

if (!require("glmnet")) {
  install.packages("glmnet", repos="http://cran.rstudio.com/") 
  library("glmnet")
}

### Prepare Data ###
setwd("/Users/hansoochang/Drexel/CBT+")

library("caTools")


### TRAIN SET ### 

# Helper function for balanceData function #
getData <- function(data) {
  #predictors  
  ind <- c("lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx",
           "lagUrges",  "lagMood")
  
  MLpred<-data[,ind]
  
  table(MLpred$lagMS_LOC)
  table(MLpred$lagCompBx)
  table(MLpred$lagUrges)
  #specify the categorical variables.
  MLpred$lagMS_Type <- as.factor(MLpred$lagMS_Type)
  MLpred$lagMS_LOC <- as.numeric(MLpred$lagMS_LOC )
  MLpred$lagCompBx <- as.numeric(MLpred$lagCompBx)
  MLpred$lagUrges <- as.numeric(MLpred$lagUrges)
  
  table(MLpred$lagMS_LOC)
  table(MLpred$lagCompBx)
  table(MLpred$lagUrges)
  
  rows<-nrow(MLpred)
  MLpred$TimeCat1<-matrix(0,rows,1)
  MLpred$TimeCat2<-matrix(0,rows,1)
  MLpred$TimeCat3<-matrix(0,rows,1)
  MLpred$TimeCat4<-matrix(0,rows,1)
  MLpred$TimeCat5<-matrix(0,rows,1)
  MLpred$TimeCat6<-matrix(0,rows,1)
  MLpred$TimeCat7<-matrix(0,rows,1)
  ind1<-which(MLpred$lagMS_Type==2)
  ind2<-which(MLpred$lagMS_Type==3)
  ind3<-which(MLpred$lagMS_Type==4)
  ind4<-which(MLpred$lagMS_Type==5)
  ind5<-which(MLpred$lagMS_Type==6)
  ind6<-which(MLpred$lagMS_Type==7)
  ind7<-which(MLpred$lagMS_Type==8)
  MLpred$TimeCat1[ind1] <-1
  MLpred$TimeCat2[ind2] <-1
  MLpred$TimeCat3[ind3] <-1
  MLpred$TimeCat4[ind4] <-1
  MLpred$TimeCat5[ind5] <-1
  MLpred$TimeCat6[ind6] <-1
  MLpred$TimeCat7[ind7] <-1
  MLpred$lagMS_Type <- NULL
  
  ##get outcome
  ind3 <- c("MS_LOC")
  Binge <- data[,ind3] 
  Binge <- as.factor(Binge)
  
  ##Combine outcome and predictors
  testtrain.binge<-cbind(Binge,MLpred)
  
  return(testtrain.binge)
}

# Function used to balance the data
balanceData <- function(data, method, seed, p) {
  data1.imp.bal<-ovun.sample(Binge ~ ., data=data, method=method, p=p, seed=seed)$data
  return(data1.imp.bal)
}

# Function to return logistic regression fit on Binge. Individual
trainIndData <- function(balance_data) {
  individual.fit <- glm(Binge ~ ., data = balance_data, family = binomial)
  
  return('individual.fit'=individual.fit)
}

# Function to return logistic regression fit on Binge. Group
trainGroupData <- function(balance_data) {
  group.fit <- glm(Binge ~ ., data = balance_data, family = binomial)
  
  save(file="/Users/hansoochang/Drexel/CBT+/model/modelfile_group.binge", group.fit)
}

trainGroupDataSVM <- function(balance_data) {
  # group.fit <- glm(Binge ~ ., data = balance_data, family = binomial)
  set.seed(1)
  
  out <- svm(Binge ~ ., data = balance_data, kernel = "radial",  
             gamma = 1, cost = 1)
  
  tune.out <- tune(svm, Binge ~ ., data = balance_data,
                   kernel = "radial",
                   ranges = list(
                     cost = c(0.1, 1, 10, 100, 1000),
                     gamma = c(0.5, 1, 2, 3, 4)
                   )
  )
  
  bestmod <- tune.out$best.model
  
  save(file="/Users/hansoochang/Drexel/CBT+/model/modelfile_group.binge", group.fit)
}

### Show the confusion matrix of training set (Accuracy, Specificity, and Sensitivity of fit and data) ###
showAccSpecSens <- function (fit, data) {
  glm.probs <- predict (fit, type = "response")
  
  contrasts(data$Binge)
  
  glm.pred <- rep(0, nrow(data))
  glm.pred[glm.probs > .5] = 1
  
  return(confusionMatrix(table(glm.pred, data$Binge), positive = "1"))
}

### For SVM
showAccSpecSensSVM <- function (fit, data) {
  glm.probs <- predict (fit, type = "response")
  
  contrasts(data$Binge)
  
  glm.pred <- rep(0, nrow(data))
  glm.pred[glm.probs > .5] = 1
  
  return(confusionMatrix(table(glm.pred, data$Binge), positive = "1"))
}

### show the confusion matrix of test set (Accuracy, Specificity, and Sensitivity of fit and data)
predictAccSpecSens <- function (fit, data) {
  glm.probs <- predict(fit, newdata = data, type = "response")
  
  
  contrasts(data$Binge)
  
  glm.pred <- rep(0, nrow(data))
  glm.pred[glm.probs > .5] = 1
  
  return(confusionMatrix(table(glm.pred, data$Binge), positive = "1"))
}

trainingMatrix <- function(trainData, seed, p) {
  print(showAccSpecSens(
    trainIndData(balanceData(getData(trainData[trainData$ParticipantID %in% Good, ]), seed, p)), 
    # trainIndData(getData(trainData[trainData$ParticipantID %in% Good, ])), 
    balanceData(getData(trainData[trainData$ParticipantID %in% Good, ]), seed, p)))
    # getData(trainData[trainData$ParticipantID %in% Good, ])))
}

testMatrix <- function(trainData, testData, seed, p) {
  balanced <- balanceData(getData(trainData[trainData$ParticipantID %in% Good, ]), seed, p)
  # balanced <- getData(trainData[trainData$ParticipantID %in% Good, ])
  print(predictAccSpecSens(trainIndData(balanced), getData(testData[testData$ParticipantID %in% Good, ])))
}
 
trainingMatrixSVM <- function(trainData, seed, p) {
  print(showAccSpecSens(
    trainGroupDataSVM(balanceData(getData(trainData[trainData$ParticipantID %in% Good, ]), seed, p)), 
    balanceData(getData(trainData[trainData$ParticipantID %in% Good, ]), seed, p)))
}

testMatrixSVM <- function(trainData, testData, seed, p) {
  balanced <- balanceData(getData(trainData[trainData$ParticipantID %in% Good, ]), seed, p)
  print(predictAccSpecSens(trainIndData(balanced), getData(testData[testData$ParticipantID %in% Good, ])))
}





