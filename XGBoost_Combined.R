library(dplyr)
library(ROSE)
library(caret)
library(e1071)
library(dplyr)
library(ROSE)
library(caretEnsemble)
library(randomForest)
library(xgboost)


getData <- function(data) {
  
  #predictors  
  ind <- c("lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx",
           "lagUrges",  "lagMood", "ParticipantID","days_from_end")
  
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

acc_res<-function (yhat, y) {
  a<-length(which(yhat==y&y=="No") )
  b<-length(which(yhat!=y&y=="Yes") )
  c<-length(which(yhat!=y&y=="No") )
  d<-length(which(yhat==y&y=="Yes") )
  speci<-a/(a+c)
  sensi<-d/(b+d)
  acc<-(a+d)/length(y)
  temp<-c(speci, sensi, acc)
  return(temp)
}

LOC_All <- read.csv("CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_LOC.csv", 
                    header = TRUE, sep = ",", stringsAsFactors = T)

head(LOC_All)

LOC_All[, "MS_LOC"] <- factor(LOC_All[, "MS_LOC"])
LOC_All[, "lagMS_Type"] <- factor(LOC_All[, "lagMS_Type"])
LOC_All[, "lagMS_LOC"] <- factor(LOC_All[, "lagMS_LOC"])
LOC_All[, "lagCompBx"] <- factor(LOC_All[, "lagCompBx"])
LOC_All[, "lagUrges"] <- factor(LOC_All[, "lagUrges"])
LOC_All[, "lagMood"] <- factor(LOC_All[, "lagMood"])
LOC_All[, "lagMS_TimeElapsed"] <- factor(LOC_All[, "lagMS_TimeElapsed"])

# levels(LOC_All$MS_LOC)=c("No","Yes")

as.character(LOC_All$Entry_DateTime)
LOC_All$date_entry_string <- substring(as.character(LOC_All$Entry_DateTime), 1, nchar(as.character(LOC_All$Entry_DateTime))-5)
LOC_All$date_entry_string <- trimws(LOC_All$date_entry_string)

LOC_All$formatted_entry_date <- as.Date(LOC_All$date_entry_string, "%m/%d/%Y")

LOC_All <- LOC_All %>% 
  group_by(ParticipantID) %>% 
  mutate(enddate = max(formatted_entry_date)) %>%
  ungroup()


LOC_All <- as.data.frame(LOC_All)

LOC_All$days_from_end <- LOC_All$formatted_entry_date - LOC_All$enddate

#remove subjects with too few data points
IDrm <- c("2028", "2035","2043", "2063", "2064", "2066","2067", "2068", "2081", "2110", "2122", "2126", "2139")
LOC_All = LOC_All[-which(LOC_All$ParticipantID%in%IDrm),]

IDbyweeks <- LOC_All %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(MS_LOC=="Yes")) ,
            nonLOC = length(which(MS_LOC=="No")) )
View(IDbyweeks)
#training and test sets split
#train <- LOC_All[abs(LOC_All$days_from_end) > 49, ]
#test <- LOC_All[abs(LOC_All$days_from_end) <= 49, ]

indx = unique(LOC_All$ParticipantID)
m = length(indx)
indxTest = NULL
#reschecking = NULL
for (i in 1:m){
  indx1 = which(LOC_All$ParticipantID == indx[i] &LOC_All$MS_LOC == 1)
  indx2 = which(LOC_All$ParticipantID == indx[i] &LOC_All$MS_LOC == 0)
  prop1 = round (length(indx1) * 0.25)
  prop2 = round (length(indx2) * 0.25)
  set.seed(i)
  temp1 = sample(indx1, size = prop1)
  temp2 = sample(indx2, size = prop2)
  indxTest = c(indxTest , temp1, temp2)
  
  #indxx=c(indx1,indx2)
  #temp=c(temp1,temp2)
  #res= c(indx[i], table(LOC_All[indxx,]$MS_LOC)[1]/table(LOC_All[indxx,]$MS_LOC)[2],
  #  table(LOC_All[temp,]$MS_LOC)[1]/table(LOC_All[temp,]$MS_LOC)[2])
  #reschecking = rbind(reschecking, res)
}
train <- LOC_All[-indxTest, ]
test <- LOC_All[indxTest, ]

nrow(train)
table(train$MS_LOC)
#No  Yes 
#7658 1182 
nrow(test)
table(test$MS_LOC)
#No  Yes 
#2554  156 

IDbyweeks_train <- train %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(MS_LOC=="Yes")) ,
            nonLOC = length(which(MS_LOC=="No")) )
View(IDbyweeks_train)
IDbyweeks_test <- test %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(MS_LOC=="Yes")) ,
            nonLOC = length(which(MS_LOC=="No")) )
View(IDbyweeks_test)

LOCtrain = getData(train)
LOCtest = getData(test)
LOCtest.x = LOCtest[, -1]

# resampling the training set
LOCtrain.bal<-ovun.sample(Binge ~ . , data=LOCtrain, method="under", p=0.50, seed=5)$data
table(LOCtrain.bal$Binge)

IDbyweeks_trainbal <- LOCtrain.bal %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(Binge=="Yes")) ,
            nonLOC = length(which(Binge=="No")) )
View(IDbyweeks_trainbal)


### XGBoost ###
### Might have the change the proportion here
ind <- c("lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx",
         "lagUrges",  "lagMood", "ParticipantID")

dummy <- dummyVars(" ~ .", data=ranTrain[, ind])
newdata <- data.frame(predict(dummy, newdata = ranTrain[, ind])) 
data_train <- cbind(newdata, ranTrain[, "MS_LOC"])
data_train
dummy.test <- dummyVars(" ~ .", data=ranTest[, ind])
newdata.test <- data.frame(predict(dummy.test, newdata = ranTest[, ind])) 
data_test <- cbind(newdata.test, ranTest[, "MS_LOC"])


data_train$Binge <- data_train$`ranTrain[, "MS_LOC"]`
data_train <- subset(data_train, select = -`ranTrain[, "MS_LOC"]`)
data_train

data_test$Binge <- data_test$`ranTest[, "MS_LOC"]`
data_test <- subset(data_test, select = -`ranTest[, "MS_LOC"]`)
data_test


indx = unique(data_train$ParticipantID)
indx
m = length(indx)
testResults = NULL
LOCtrain <- data_train
LOCtest <- data_test
for (itarget in 1:m){
  indx1 = which(LOCtrain$ParticipantID == indx[itarget] &LOCtrain$Binge == 1)
  indx2 = which(LOCtrain$ParticipantID == indx[itarget] &LOCtrain$Binge == 0)
  #take 50% of data from this target user as a validation set to find an optimal weight
  prop1 = round (length(indx1) * 0.5)
  prop2 = round (length(indx2) * 0.5)
  set.seed(123)
  temp1 = sample(indx1, size = prop1)
  temp2 = sample(indx2, size = prop2)
  tempp = c(temp1, temp2)
  trainsub <- LOCtrain[-tempp, ]
  # validation set is used to find an optimal weight
  validationset <- LOCtrain[tempp, ]
  # this set is not balanced, more non-LOC than LOC
  table(validationset$Binge)
  
  
  ### use the trainsub data to build a group-level mode
  ##### undersampling the trainsub data to create a balanced set
  trainsub.bal = NULL
  indxsub = unique(trainsub$ParticipantID)
  k = length(indxsub)
  for (j in 1:k){
    indxsub1 = which(trainsub$ParticipantID == indxsub[j] )
    indxsub1no = which(trainsub$ParticipantID == indxsub[j] 
                       & trainsub$Binge == 0   )
    indxsub1yes = which(trainsub$ParticipantID == indxsub[j] 
                        & trainsub$Binge == 1   )
    # number of non-LOC cases
    ms1 = length(indxsub1no)
    # number of LOC cases
    ms2 = length(indxsub1yes)
    if(ms1 > ms2) {
      set.seed(j)
      temp1 = sample(indxsub1no, size = ms2)
      indxs = c(temp1, indxsub1yes)
      trainsub.bal = rbind(trainsub.bal, trainsub[indxs, ])
    } else {
      trainsub.bal = rbind(trainsub.bal, trainsub[indxsub1, ])
    }
  }
  # the group balanced training data that can be used to build the group levle model
  table(trainsub.bal$Binge)
  #the individual balanced training data that can be used to build the individual model
  individualtrain <- trainsub.bal[which( trainsub.bal$ParticipantID == indx[itarget]), ]
  table(individualtrain$Binge)
  
  ##### build the group level ensemble learning using trainsub.bal
  set.seed(123)
  myControl <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            search = "grid",
                            savePredictions = "final",
                            index = createResample(trainsub.bal$Binge, 10),
                            
                            classProbs = TRUE,
                            verboseIter = TRUE)
  
  model_list <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                            TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + days_from_end , data=trainsub.bal, trControl = myControl,  
                          methodList =c( "rf","nnet","glm")
  )
  modelgroup <- caretEnsemble(model_list)
  
  #performance on the training set
  table(predict(modelgroup, trainsub.bal[, -1]), trainsub.bal$Binge)
  grouptrain=acc_res(predict(modelgroup, trainsub.bal[, -1]), trainsub.bal$Binge)
  
  
  ############## the individual level model
  set.seed(123)
  myControl1 <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3,
                             search = "grid",
                             savePredictions = "final",
                             index = createResample(individualtrain$Binge, 10),
                             
                             classProbs = TRUE,
                             verboseIter = TRUE)
  
  model_list1 <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                             TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + days_from_end , 
                           data= individualtrain, trControl = myControl1,  
                           methodList =c( "rf","nnet","glm")
  )
  modelindividual <- caretEnsemble(model_list1)
  
  #performance on the training set
  table(predict(modelindividual, individualtrain[, -1]), individualtrain$Binge)
  individualtrain=acc_res(predict(modelindividual, individualtrain[, -1]), individualtrain$Binge)
  
  ###### use the validation set to find an optimal weight
  #group-level model
  yhat = predict(modelgroup, validationset[, -1], type="prob")
  #individual-level model
  yhat2 = predict(modelindividual,  validationset[, -1], type="prob")
  
  ### choose the weight
  all.ycomb=NULL
  
  #use different weights
  #if probability >=0.5, No
  #if probability <0.5, Yes
  k=10
  for (i in 0:k){
    yprob=yhat*i/10+yhat2*(10-i)/10
    
    n=length(yprob)
    ycomb<-NULL
    for (j in 1:n)
    {
      #if probability <0.5, Yes
      #if probability >=0.5, No
      if (yprob[j] <0.5 | is.na(yprob[j])==T) ytemp="Yes"
      else ytemp="No"
      ycomb<-c(ycomb,ytemp)
    }
    all.ycomb<-cbind(all.ycomb,ycomb)
  }
  
  
  results.wt<-NULL
  true.y<- validationset$Binge
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
    ind<-which.max(results.wt[temp2,3])
    weight.binge<-results.wt[temp2,4][ind] - 1
  } else{
    
    temp3<-which.max(results.wt[,3])
    weight.binge<-results.wt[ ,4][temp3] - 1
  }
  # the weight identified
  if(length(weight.binge)>1){ 
    weight.binge = weight.binge[1]
  }
  
  print(weight.binge)
  
  #### apply the combined model to the test set
  # the target individual's data in the test set
  individualtest = LOCtest[which( LOCtest$ParticipantID == indx[itarget]), ]
  table(individualtest$Binge)
  # gropu level prediction  
  yhattest = predict(modelgroup, individualtest[, -1],type="prob")
  # individual level prediction
  yhattest2 = predict(modelindividual, individualtest[, -1],type="prob")
  
  # combine prediction by estimated weight
  weight.binge<-as.numeric(weight.binge)
  yprobtest=yhattest*weight.binge/10+yhattest2*(10-weight.binge)/10
  temptest = cbind(individualtest, yprobtest, weight.binge,grouptrain[1], grouptrain[2],grouptrain[3],individualtrain[1], individualtrain[2],individualtrain[3])
  testResults = rbind(testResults, temptest)
  
}






table(data_train_bal$Binge)
table(data_test$Binge)

grid_tune <- expand.grid(
  nrounds = c(100, 500, 1000), #number of trees
  max_depth = c(2,4,6),
  eta = 0.1, #c(0.025,0.05,0.1,0.3), #Learning rate
  gamma = 1, # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 0.6,
  min_child_weight = 3, # the larger, the more conservative the model
  #is; can be used as a stop
  subsample = 0.5 # used to prevent overfitting by sampling X% training
)

train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)


xgb_tune <- train(x = data_train_bal[,1:25],
                  y = data_train_bal$Binge,
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  verbose = TRUE)
xgb_tune

xgb_tune$bestTune

# Writing out the best model.

train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)
xgb_model <- train(x = data_train_bal[,1:25],
                   y = data_train_bal$Binge,
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)

data_train_bal.x <- data_train_bal[, -c(28,27,26)]

xgb.pred.train <- predict(xgb_model, data_train_bal)

confusionMatrix(as.factor(as.numeric(xgb.pred.train)),
                as.factor(as.numeric(data_train_bal$Binge)), positive = "1")

# Prediction:
xgb.pred <- predict(xgb_model, data_test)
# Confusion Matrix
confusionMatrix(as.factor(as.numeric(xgb.pred)),
                as.factor(as.numeric(data_test$Binge)), positive = "1")

table(data_train$Binge)
table(data_test$Binge)


