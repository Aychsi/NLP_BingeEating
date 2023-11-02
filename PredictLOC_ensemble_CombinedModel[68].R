
### Data comes from RandomSplit.R

indx = unique(LOCtrain$ParticipantID)
m = length(indx)
LOCtrain.bal = NULL
for (i in 1:m){
  indx1 = which(LOCtrain$ParticipantID == indx[i] )
  indx1no = which(LOCtrain$ParticipantID == indx[i] 
                  & LOCtrain$Binge == "No"   )
  indx1yes = which(LOCtrain$ParticipantID == indx[i] 
                   & LOCtrain$Binge == "Yes"   )
  # number of non-LOC cases
  m1 = length(indx1no)
  # number of LOC cases
  m2 = length(indx1yes)
  if(m1 > m2) {
    set.seed(i)
    temp1 = sample(indx1no, size = m2)
    indxs = c(temp1, indx1yes)
    LOCtrain.bal = rbind(LOCtrain.bal, LOCtrain[indxs, ])
  } else {
    LOCtrain.bal = rbind(LOCtrain.bal, LOCtrain[indx1, ])
  }
}

table(LOCtrain.bal$Binge)
#No  Yes 
#956 1001 
#821 821


indx = unique(LOCtrain$ParticipantID)
m = length(indx)
testResults = NULL
for (itarget in 1:m){
  indx1 = which(LOCtrain$ParticipantID == indx[itarget] &LOCtrain$Binge == "Yes")
  indx2 = which(LOCtrain$ParticipantID == indx[itarget] &LOCtrain$Binge == "No")
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
                    & trainsub$Binge == "No"   )
    indxsub1yes = which(trainsub$ParticipantID == indxsub[j] 
                     & trainsub$Binge == "Yes"   )
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
  
  model_list <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + 
                          lagMS_AteEnough +lagMS_Macros+lagMS_FoodRule+lagMS_NoneAbove+lagUrgeStrat+
                          lagUrgeStratHelp+lagMood_Strategy + days_from_end , data=trainsub.bal, trControl = myControl,  
                          methodList =c( "rf","nnet", "xgbTree", "xgbLinear")
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
  
  model_list1 <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + 
                             lagMS_AteEnough +lagMS_Macros+lagMS_FoodRule+lagMS_NoneAbove+lagUrgeStrat+
                             lagUrgeStratHelp+lagMood_Strategy + days_from_end , 
                          data= individualtrain, trControl = myControl1,  
                          methodList =c( "rf","nnet", "xgbTree", "xgbLinear")
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

#if probability <0.5, Yes
#if probability >=0.5, No
predytest = testResults$yprobtest
predytest[which(testResults$yprobtest >= 0.5)] = "No"
predytest[which(testResults$yprobtest < 0.5)] = "Yes"
#performance on the test set
table(predytest, LOCtest$Binge)
acc_res(predytest, LOCtest$Binge)
# Speci      Sensi      Acc
#0.6721968 0.6975089 0.6757023 LOC_All
#0.6539141 0.6545455 0.6540020 LOC_All_Long
#0.6252860 0.6832740 0.6333169 LOC_All_new
confusionMatrix(table(predytest, LOCtest$Binge), positive ="Yes")







