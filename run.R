if (!require("e1071")) {
  install.packages("e1071", repos="http://cran.rstudio.com/") 
  library("e1071")
}

trainingMatrix(7, .50)
testMatrix(7, .50)

### This one is best
trainingMatrix(train, 18, .55)
testMatrix(train, test, 18, .55)

trainingMatrix(7, .60)
testMatrix(7, .60)


### SVM Group + Individual Model Last Two Weeks Test Set ###
getGroupModelsSVM(7, 0.50, 0.50)
matrix <- getPredictionMatrixSVM(train, test)
confusionMatrix(matrix, positive = "1")

### Logistic Regression Group + Individual Model Last Two Weeks Test Set ###
sortData(ranTrain, ranTest)
getGroupModels(ranTrain, "under", 7, 0.50)
getPredictionMatrix(ranTrain, ranTest)


### Logistic Regression Group + Individual Model Random Test Set (25%) ###
getGroupModels(train, 7, 0.55, 0.55)
getPredictionMatrix(train, test)

### SVM Group + Individual Model Last Two Weeks Test Set ###
getGroupModelsSVM(7, 0.50, 0.50)
matrix <- getPredictionMatrixSVM()
confusionMatrix(matrix, positive = "1")

