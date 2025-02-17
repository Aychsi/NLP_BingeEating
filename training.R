#' <!-- `rmarkdown::render('2020_PD_data_age_matched_test2.R')` -->




load_obj = function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}




#########################
#install packages


source ('model_path.R')

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

load_obj = function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

options(warn=-1)



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
  Binge<-data[,ind3] 
  Binge <- as.factor(Binge)
  
  ##Combine outcome and predictors
  testdata.binge<-cbind(Binge,MLpred)
  
  return(testdata.binge)
}


##########################################################################
###Train Individual ML model for BINGE##
trainIndividualBinge <- function(data) {
  
  data$Binge <- as.factor(data$Binge)
  
  data1.imp.bal<-ovun.sample(Binge ~ ., data=data, method="both", p=0.55, seed=5)$data
  
  
  ############# Individual classification ############
  
  individual.fit <- glm(
    Binge ~ .,
    data = data1.imp.bal, family = binomial)
  
  
  return(list('individual.fit'=individual.fit,  'data1impbal'=data1.imp.bal))
}

trainIndividualBingeSVM <- function(data) {
  
  data$Binge <- as.factor(data$Binge)
  
  data1.imp.bal<-ovun.sample(Binge ~ ., data=data, method="both", p=0.50, seed=7)$data
  
  
  ############# Individual classification ############
  
  set.seed(1)
  
  out <- svm(Binge ~ ., data = data1.imp.bal, kernel = "radial",  
             gamma = 1, cost = 1, probability = TRUE)
  
  tune.out <- tune(svm, Binge ~ ., data = data1.imp.bal,
                   kernel = "radial",
                   ranges = list(
                     cost = c(0.1, 1, 10, 100, 1000),
                     gamma = c(0.5, 1, 2, 3, 4)
                   ), probability = T
  )
  
  bestmod <- tune.out$best.model
  
  
  return(list('individual.fit'=bestmod,  'data1impbal'=data1.imp.bal))
}

###getWeights BINGE####
getWeightsBingeSVM <- function(userData, data1impbal, individualfit.binge, ind) {
  data1.imp.bal.ind <- data1impbal
  
  testdata.binge <- userData
  
  testdata.binge$Binge <- as.factor(testdata.binge$Binge)
  
  ##Make a test dataset with only predictors
  testdata3.x<-testdata.binge[,-1]
  ##Make a test dataset with only the predicted values
  testdata3.y<-testdata.binge[,1]
  
  
  acc_res<-function (yhat, y) {
    a<-length(which(yhat==y&y==0) )
    b<-length(which(yhat!=y&y==1) )
    c<-length(which(yhat!=y&y==0) )
    d<-length(which(yhat==y&y==1) )
    speci<-a/(a+c)
    sensi<-d/(b+d)
    acc<-(a+d)/length(y)
    temp<-c(speci, sensi, acc)
    return(temp)
  }
  
  #### load group-level model  ####
  modelfile_group.binge <- load_obj(paste(modelPathSVM, ind, sep=''))
  fit.binge<- modelfile_group.binge 
  ## the individual level model ####
  fit2.binge <- individualfit.binge
  
  
  ### use validation data to choose the weight
  validation_set<-testdata3.x
  #group-level model
  yhat <- predict(fit.binge,validation_set,probability = TRUE)
  yhatProb <- attr(yhat,"probabilities")[,2] 
  #individual-level model
  yhat2 <- predict(fit2.binge,validation_set,probability = TRUE)
  yhat2Prob <- attr(yhat2,"probabilities")[,2]
  
  ### choose the weight
  all.ycomb=NULL
  
  #use different weights
  k=10
  for (i in 0:k){
    yprob=yhatProb*i/10+yhat2Prob*(10-i)/10
    
    n=length(yprob)
    ycomb<-NULL
    for (j in 1:n)
    {
      
      if (yprob[j] <0.5 | is.na(yprob[j])==T) ytemp=0
      else ytemp=1
      ycomb<-c(ycomb,ytemp)
    }
    all.ycomb<-cbind(all.ycomb,ycomb)
  }
  
  
  results.wt<-NULL
  true.y<-as.numeric(testdata3.y)-1
  mm<-ncol(all.ycomb)
  for (i in 1:mm){
    #need to add a few more commands here to select the optimal weight based on classification performance
    temp<-c( acc_res(all.ycomb[,i], true.y) ,i )
    results.wt<-rbind( results.wt,temp)
  }
  
  #specificity, sensitivity, accuracy
  #the weight should be chosen based on this output
  weight.binge<-0
  temp<- which(results.wt[,1]>=0.5)
  if(length(temp)>0){
    max.sensi<-max(results.wt[temp,2])
    temp2<-which(results.wt[,2]==max.sensi)
    inde<-which.max(results.wt[temp2,3])
    eight.binge<-results.wt[temp2,4][inde] - 1
  } else{
    
    temp3<-which.max(results.wt[,3])
    weight.binge<-results.wt[ ,4][temp3] - 1
  }
  a <- list("ID" = ind, "Weight" = weight.binge)
  filea <- paste(modelPathWeights, ind, sep='')
  save(file = filea , a)
  weight.binge
  
  return(list('modelfile_group.binge'=fit.binge, 'modelfile_ind.binge'=fit2.binge, 'weight.binge'=weight.binge))
}


###getWeights BINGE####
getWeightsBinge <- function(userData, data1impbal, individualfit.binge, ind) {
  data1.imp.bal.ind <- data1impbal
  
  testdata.binge <- userData
  
  testdata.binge$Binge <- as.factor(testdata.binge$Binge)
  
  ##Make a test dataset with only predictors
  testdata3.x<-testdata.binge[,-1]
  ##Make a test dataset with only the predicted values
  testdata3.y<-testdata.binge[,1]
  
  
  acc_res<-function (yhat, y) {
    a<-length(which(yhat==y&y==0) )
    b<-length(which(yhat!=y&y==1) )
    c<-length(which(yhat!=y&y==0) )
    d<-length(which(yhat==y&y==1) )
    speci<-a/(a+c)
    sensi<-d/(b+d)
    acc<-(a+d)/length(y)
    temp<-c(speci, sensi, acc)
    return(temp)
  }
  
  #### load group-level model  ####
  modelfile_group.binge <- load_obj(paste(modelPath, ind, sep=''))
  fit.binge<- modelfile_group.binge 
  ## the individual level model ####
  fit2.binge <- individualfit.binge
  
  
  ### use validation data to choose the weight
  validation_set<-testdata3.x
  #group-level model
  yhat <- predict(fit.binge,validation_set, type = "response")

  #individual-level model
  yhat2 <- predict(fit2.binge,validation_set, type = "response")

  
  ### choose the weight
  all.ycomb=NULL
  
  #use different weights
  k=10
  for (i in 0:k){
    yprob=yhat*i/10+yhat2*(10-i)/10
    
    n=length(yprob)
    ycomb<-NULL
    for (j in 1:n)
    {
      
      if (yprob[j] <0.5 | is.na(yprob[j])==T) ytemp=0
      else ytemp=1
      ycomb<-c(ycomb,ytemp)
    }
    all.ycomb<-cbind(all.ycomb,ycomb)
  }
  
  
  results.wt<-NULL
  true.y<-as.numeric(testdata3.y)-1
  mm<-ncol(all.ycomb)
  for (i in 1:mm){
    #need to add a few more commands here to select the optimal weight based on classification performance
    temp<-c( acc_res(all.ycomb[,i], true.y) ,i )
    results.wt<-rbind( results.wt,temp)
  }
  
  #specificity, sensitivity, accuracy
  #the weight should be chosen based on this output
  weight.binge<-0
  temp<- which(results.wt[,1]>=0.5)
  if(length(temp)>0){
    max.sensi<-max(results.wt[temp,2])
    temp2<-which(results.wt[,2]==max.sensi)
    inde<-which.max(results.wt[temp2,3])
    weight.binge<-results.wt[temp2,4][inde] - 1
  } else{
    
    temp3<-which.max(results.wt[,3])
    weight.binge<-results.wt[ ,4][temp3] - 1
  }
  a <- list("ID" = ind, "Weight" = weight.binge)
  filea <- paste(modelPathWeights, ind, sep='')
  save(file = filea , a)
  weight.binge
  
  return(list('modelfile_group.binge'=fit.binge, 'modelfile_ind.binge'=fit2.binge, 'weight.binge'=weight.binge))
}

individualPredictionBinge <- function(testdata.binge, modelfile_ind.binge, weight.binge) {
  
  ##make a predictor only dataset
  testdata3.x<-testdata.binge[,-1]
  ##make an outcome only dataset
  y<-testdata.binge[,1]
  
  #### load group-level model  ####
  modelfile_group.binge <- load_obj(paste(modelPath, "/modelfile_group.binge", sep=''))
  fit.binge<- modelfile_group.binge 
  ## the individual level model ####
  fit2.binge <- modelfile_ind.binge
  
  # gropu level prediction  
  yhat = predict(fit.binge, testdata3.x,type="response")
  # individual level prediction
  yhat2 = predict(fit2.binge,testdata3.x,type="response")
  
  # combine prediction by estimated weight
  weight.binge<-as.numeric(weight.binge)
  yprob=yhat*weight.binge/10+yhat2*(10-weight.binge)/10
  
  ## these cutoff values can be changed
  if (yprob<= 0.3) {
    # no risk
    alert=0
  } else if (0.3<yprob & yprob<=0.5) {
    # small
    alert=1
  } else if(0.5<yprob & yprob<=0.8) {
    # medium
    alert=2
  } else {
    # high
    alert=3
  }
  
  #### top risky factors  ####
  fit.coef.binge<-coef(fit.binge)
  fit.coef2.binge<-coef(fit2.binge)
  
  #the codes below give the top risky factors for this input data vector  
  m<-length(fit.coef.binge)
  value<-NULL
  for (i in 1:m-1){
    if (is.na(fit.coef2.binge[i+1])==T){
      temp<-fit.coef.binge[i+1]* testdata3.x[i]
    } else{
      temp<-fit.coef.binge[i+1]* testdata3.x[i]*weight.binge/10  + fit.coef2.binge[i+1]* testdata3.x[i]*(10-weight.binge)/10
    }
    value<-c(value,temp)
  }
  
  #value <- as.numeric(value)
  #names(value)<-names(testdata3.x)
  #value_ordered<-sort(unlist(value),decreasing=T)
  hello <- apply(as.data.frame(value), 2, sort, decreasing = T)
  value_ordered <- sort(hello[1, ], decreasing = T)[1:3]
  
  #remove negative coefficients
  risky_factor<-value_ordered[1:3]
  temp<-which(risky_factor>0)
  risky_factor_reported<-risky_factor[temp]
  if(length(temp)==0 | alert==0) {
    return(NULL);
  } else{
    return(list('data'=risky_factor_reported, 'alert'=alert))
  }
  
}

individualPredictionMatrix <- function(testdata.binge, modelfile_ind.binge, weight.binge, ind) {
  
  ##make a predictor only dataset
  testdata3.x<-testdata.binge[,-1]
  ##make an outcome only dataset
  y<-testdata.binge[,1]
  
  #### load group-level model  ####
  modelfile_group.binge <- load_obj(paste(modelPath, toString(ind), sep=''))
  fit.binge<- modelfile_group.binge 
  ## the individual level model ####
  fit2.binge <- modelfile_ind.binge
  
  # gropu level prediction  
  yhat = predict(fit.binge, testdata3.x, type = "response")
  # individual level prediction
  yhat2 = predict(fit2.binge,testdata3.x, type = "response")
  
  # combine prediction by estimated weight
  weight.binge<-as.numeric(weight.binge)
  yprob=yhat*weight.binge/10+yhat2*(10-weight.binge)/10
  
  
  contrasts(as.factor(testdata.binge$Binge))
  
  pred <- rep(0, nrow(testdata.binge))
  pred[yprob > .5] = 1
  
  return(list(pred, testdata.binge$Binge))
  #return(confusionMatrix(table(pred, testdata.binge$Binge), positive = as.character(1)))
  
}

individualPredictionMatrixSVM <- function(testdata.binge, modelfile_ind.binge, weight.binge, ind) {
  
  ##make a predictor only dataset
  testdata3.x<-testdata.binge[,-1]
  ##make an outcome only dataset
  y<-testdata.binge[,1]
  
  #### load group-level model  ####
  modelfile_group.binge <- load_obj(paste(modelPathSVM, toString(ind), sep=''))
  fit.binge<- modelfile_group.binge 
  ## the individual level model ####
  fit2.binge <- modelfile_ind.binge
  
  # gropu level prediction  
  yhat_prior = predict(fit.binge, testdata3.x, probability = T)
  yhat <- attr(yhat_prior,"probabilities")[,2]
  # individual level prediction
  yhat2_prior = predict(fit2.binge,testdata3.x,probability = T)
  yhat2 <- attr(yhat2_prior,"probabilities")[,2]
  # combine prediction by estimated weight
  weight.binge<-as.numeric(weight.binge)
  yprob=yhat*weight.binge/10+yhat2*(10-weight.binge)/10
  
  
  length(na.omit(testdata.binge)$Binge) # this might be wrong
  pred <- rep(0, length(na.omit(testdata.binge)$Binge))
  pred[yprob > .5] = 1
  confusionMatrix(table(factor(pred, levels = c("0", "1")), na.omit(testdata.binge)$Binge), positive = as.character(1))
  
}


groupPredictionBinge <- function(testdata.binge) {
  
  ##make a predictor only dataset
  testdata3.x<-testdata.binge[,-1]
  ##make an outcome only dataset
  y<-testdata.binge[,1]
  
  #### load group-level model  ####
  modelfile_group.binge <- load_obj(paste(modelPath, "/modelfile_group.binge", sep=''))
  fit.binge<- modelfile_group.binge
  
  
  # gropu level prediction  
  yhat = predict(fit.binge, testdata3.x,type="response")
  
  yprob=yhat
  
  contrasts(as.factor(testdata.binge$Binge))
  
  pred <- rep(0, nrow(testdata.binge))
  pred[yprob > .5] = 1
  
  return(confusionMatrix(table(pred, testdata.binge$Binge), positive = as.character(1)))
}






#predict function call
makePredictionSVM <- function(userData, survey, ind) {
  #userData <- read.csv(text=userData, header=TRUE, na.strings ="#NULL!")
  #survey <- read.csv(text=survey, header=TRUE, na.strings ="#NULL!")
  
  parsedSurvey <- getData(survey)
  
  prediction <- NULL
  
  tryCatch({
    i<-is.na(userData$MS_LOC)
    userData<-userData[!i, ]
    
    parsedUserData <- getData(userData)
    
    set.seed(1)
    n=nrow(parsedUserData)
    individualTrainingData = sample(1:n, 2*n/3)
    estimationData = c(1:n)[-individualTrainingData]
    
    
    individualTrainingData <- parsedUserData[individualTrainingData,]
    estimationData <- parsedUserData[estimationData,]
    
    ### If there are no 1's in Binge, then it will cause error in SVM function.
    for (s in 2:11) {
      while (length(unique(individualTrainingData$Binge)) < 2) {
        set.seed(s)
        n=nrow(parsedUserData)
        individualTrainingData = sample(1:n, 2*n/3)
        estimationData = c(1:n)[-individualTrainingData]
        
        individualTrainingData <- parsedUserData[individualTrainingData,]
        estimationData <- parsedUserData[estimationData,]
      }
    }  
    
    individual = trainIndividualBingeSVM(individualTrainingData)
    
    individualfit <- individual$individual.fit
    
    tryCatch({
      estimation = getWeightsBingeSVM(estimationData, individual$data1impbal, individualfit, ind)
      
      modelfile_ind.binge <- estimation$modelfile_ind.binge
      
      weight.binge <- estimation$weight.binge
      testdata.binge <- parsedSurvey
      
      # we only want to use this one
      prediction <- individualPredictionMatrixSVM(testdata.binge,  modelfile_ind.binge, weight.binge, ind)
    }, error = function(errorcondition) {
      print(paste('got estimation error', ind))
      print(errorcondition)
      
      prediction <- individualPredictionMatrixSVM(testdata.binge,  modelfile_ind.binge, weight.binge, ind)
    })
  }, error = function(errorcondition) {
    print(paste('got individual error', ind))
    print(errorcondition)
    
    prediction <- individualPredictionMatrixSVM(testdata.binge,  modelfile_ind.binge, weight.binge, ind)
  })
  
  return(prediction)
}

makePrediction <- function(userData, survey, ind) {
  #userData <- read.csv(text=userData, header=TRUE, na.strings ="#NULL!")
  #survey <- read.csv(text=survey, header=TRUE, na.strings ="#NULL!")
  
  parsedSurvey <- getData(survey)
  
  prediction <- NULL
  
  tryCatch({
    i<-is.na(userData$MS_LOC)
    userData<-userData[!i, ]
    
    parsedUserData <- getData(userData)
    
    set.seed(1)
    n=nrow(parsedUserData)
    individualTrainingData = sample(1:n, 2*n/3)
    estimationData = c(1:n)[-individualTrainingData]
    
    individualTrainingData <- balanceData(parsedUserData[individualTrainingData,],"both", 7, 0.50)
    estimationData <- balanceData(parsedUserData[estimationData,],"both", 7, 0.50)
    
    individual = trainIndividualBinge(individualTrainingData)
    
    individualfit <- individual$individual.fit
    
    tryCatch({
      estimation = getWeightsBinge(estimationData, individual$data1impbal, individualfit, ind)
      
      modelfile_ind.binge <- estimation$modelfile_ind.binge
      
      weight.binge <- estimation$weight.binge
      testdata.binge <- parsedSurvey
      
      # we only want to use this one
      prediction <- individualPredictionMatrix(testdata.binge,  modelfile_ind.binge, weight.binge, ind)
    }, error = function(errorcondition) {
      print(paste('got estimation error', ind))
      print(errorcondition)
      
      prediction <- individualPredictionMatrix(testdata.binge,  modelfile_ind.binge, weight.binge, ind)
    })
  }, error = function(errorcondition) {
    print(paste('got individual error', ind))
    print(errorcondition)
    
    prediction <- individualPredictionMatrix(testdata.binge,  modelfile_ind.binge, weight.binge, ind)
  })
  
  return(prediction)
}


getPredictionMatrixSVM <- function (train, test) {
  #predicted <- list()
  #true <- c()
  hold <- 0
  for (ind in Good) {
    
    training <- train[train$ParticipantID == ind, ]
    testing <- test[test$ParticipantID == ind, ]
    
    if (length(unique(training[training$ParticipantID == ind, ]$MS_LOC)) < 2) {
      print(ind)
    }
    
    tryCatch({
      result = makePredictionSVM(training, testing, ind)
      #predicted <- append(predicted, result[[1]])
      #true <- append(true, result[[2]])
    }, error = function(e) {
      print(paste("Error", ind))
    })
    hold <- hold + result$table
    print(hold)
    # final <- confusionMatrix(hold, positive = "1")
    # final <- confusionMatrix(data = as.factor(unlist(predicted)), reference = as.factor(unlist(true)-1), positive = "1")
  }
  return(hold)
}


getPredictionMatrix <- function (train, test) {
  predicted <- list()
  true <- c()
  
  for (ind in Good) {
    
    training <- train[train$ParticipantID == ind, ]
    testing <- test[test$ParticipantID == ind, ]
    
    tryCatch({
      result = makePrediction(training, testing, ind)
      predicted <- append(predicted, result[[1]])
      true <- append(true, result[[2]])
    }, error = function(e) {
      print(paste("Error", ind))
    })
    final <- confusionMatrix(data = as.factor(unlist(predicted)), reference = as.factor(unlist(true)-1), positive = "1")
  }
  return(final)
}


