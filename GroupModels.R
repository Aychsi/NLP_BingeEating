
sortData <- function (train, test) {
  noGood <- list()
  Good <- list()
  
  for (ID in unique(train$ParticipantID)) {
    
    if (nrow(train[train$ParticipantID == ID, ]) < 20) {
      noGood <- append(noGood, ID)
      print(paste("less than 20", toString(ID)))
      next
    } else if (length(unique(train[train$ParticipantID == ID, ]$MS_LOC)) < 2) {
      noGood <- append(noGood, ID)
      print((paste("levels", toString(ID))))
      next
    } else {
      print(ID)
      i <- train[train$ParticipantID == ID, ]
      Good <- append(Good,ID)
    }
  }
  
}


getGroupModelsSVM <- function (train, seed, indP, groupP) {
  for (ind in Good) {
    
    subject <- train[train$ParticipantID == ind, ]
    
    badIndex<-is.na(subject$MS_LOC)
    subject <-subject[!badIndex, ]
    
    parsedUserData <- getData(subject)
    
    set.seed(seed)
    n=nrow(parsedUserData)
    individualTrainingData = sample(1:n, 2*n/3) ### LOC ratio
    estimationData = c(1:n)[-individualTrainingData]
    
    individualTrainingData <- parsedUserData[individualTrainingData,]
    estimationData <- parsedUserData[estimationData,]
    
    if (length(unique(individualTrainingData$Binge)) < 2) {
      print(paste("No Binge in Training Split Data", toString(ind)))
      Good[!Good == ind]
      next
    } else {
      balance_sub <- balanceData(individualTrainingData, seed, indP) ### Changed here for Christina's codes.
      # balance_sub <- individualTrainingData For Christina's Data
      allBut <- balanceData(getData(train[!train$ParticipantID == ind, ]), seed, groupP) ### Changed here for Christina's df
      # allBut <- getData(ChrisTrain[!ChrisTrain$ParticipantID == ind, ])
      allButEst <- rbind(allBut, balance_sub)
      
      trainGroupDataSVM(allButEst, ind)
    }
  }
}


getGroupModels <- function (train, seed, groupP) { 
  for (ind in Good) {
    
    subject <- train[train$ParticipantID == ind, ]
    
    badIndex<-is.na(subject$MS_LOC)
    subject <-subject[!badIndex, ]
    
    parsedUserData <- getData(subject)
    
    set.seed(seed)
    n=nrow(parsedUserData)
    individualTrainingData = sample(1:n, 2*n/3)
    estimationData = c(1:n)[-individualTrainingData]
    
    individualTrainingData <- parsedUserData[individualTrainingData,]
    estimationData <- parsedUserData[estimationData,]
    
    if (length(unique(individualTrainingData$Binge)) < 2) {
      print(paste("No Binge in Training Split Data", toString(ind)))
      Good[!Good == ind]
      next
    } else {
      balance_sub <- individualTrainingData
      allBut <- getData(train[!train$ParticipantID == ind, ])
      allButEst <- balanceData(rbind(allBut, balance_sub), "under", seed, groupP)
      
      trainGroupData(allButEst, ind)
    }
  }
}


trainGroupDataSVM <- function(balance_data, ind) {
  set.seed(1)
  
  tune.out <- tune(svm, Binge ~ ., data = balance_data,
                   kernel = "radial",
                   ranges = list(
                     cost = c(0.1, 1, 10, 100, 1000),
                     gamma = c(0.5, 1, 2, 3, 4)
                   ), probability = TRUE
  )
  
  bestmod <- tune.out$best.model
  
  file = paste(modelPathSVM, ind, sep = "")
  print(file)
  save(file = file, bestmod)
}


trainGroupData <- function(balance_data, ind) {

  group.fit <- glm(Binge ~ ., data = balance_data, family = binomial)
  
  file = paste(modelPath, ind, sep = "")
  
  save(file = file, group.fit)
}


