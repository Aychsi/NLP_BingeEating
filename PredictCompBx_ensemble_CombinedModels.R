
# We have to take out low quality participants
IDbyweeks_train <- LOCtrain %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(CompBx=="Yes")) ,
            nonLOC = length(which(CompBx=="No")) )
View(IDbyweeks_train)

IDrm <- c("2023", "2027", "2035", "2120", "2136")
LOCtrain = LOCtrain[-which(LOCtrain$ParticipantID%in%IDrm),]

indx = unique(LOCtrain$ParticipantID)
m = length(indx)
testResults = NULL
for (itarget in 1:m){
  indx1 = which(LOCtrain$ParticipantID == indx[itarget] &LOCtrain$CompBx == "Yes")
  indx2 = which(LOCtrain$ParticipantID == indx[itarget] &LOCtrain$CompBx == "No")
  indx1
  print(indx[itarget])
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
  table(validationset$CompBx)
  
  ### use the trainsub data to build a group-level mode
  ##### undersampling the trainsub data to create a balanced set
  trainsub.bal = NULL
  indxsub = unique(trainsub$ParticipantID)
  k = length(indxsub)
  for (j in 1:k){

    indxsub1 = which(trainsub$ParticipantID == indxsub[j] )
    indxsub1no = which(trainsub$ParticipantID == indxsub[j] 
                       & trainsub$CompBx == "No"   )
    indxsub1yes = which(trainsub$ParticipantID == indxsub[j] 
                        & trainsub$CompBx == "Yes"   )
    # number of non-LOC cases
    ms1 = length(indxsub1no)
    # number of LOC cases
    ms2 = length(indxsub1yes)
    if(ms1 > ms2) {
      set.seed(j)
      # Change here to oversample. Taking a long time, so might have to undersample
      indxsub1yes
      temp1 = sample(indxsub1no, size = ms2)
      indxs = c(temp1, indxsub1yes)
      trainsub.bal = rbind(trainsub.bal, trainsub[indxs, ])
    } else {
      trainsub.bal = rbind(trainsub.bal, trainsub[indxsub1, ])
    }
  }
  # the group balanced training data that can be used to build the group levle model
  table(trainsub.bal$CompBx)
  #the individual balanced training data that can be used to build the individual model
  individualtrain <- trainsub.bal[which( trainsub.bal$ParticipantID == indx[itarget]), ]
  table(individualtrain$CompBx)
  
  ##### build the group level ensemble learning using trainsub.bal
  set.seed(123)
  myControl <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            search = "grid",
                            savePredictions = "final",
                            index = createResample(trainsub.bal$CompBx, 10),
                            
                            classProbs = TRUE,
                            verboseIter = TRUE)
  
  model_list <- caretList(CompBx ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + 
                            lagMS_AteEnough +lagMS_Macros+lagMS_FoodRule+lagMS_NoneAbove+lagUrgeStrat+
                            lagUrgeStratHelp+lagMood_Strategy + days_from_end , data=trainsub.bal, trControl = myControl,  
                          methodList =c( "rf","nnet","glm")
  )
  modelgroup <- caretEnsemble(model_list)
  
  #performance on the training set
  table(predict(modelgroup, trainsub.bal[, -1]), trainsub.bal$CompBx)
  grouptrain=acc_res(predict(modelgroup, trainsub.bal[, -1]), trainsub.bal$CompBx)
  
  
  ############## the individual level model
  set.seed(123)
  myControl1 <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3,
                             search = "grid",
                             savePredictions = "final",
                             index = createResample(individualtrain$CompBx, 10),
                             
                             classProbs = TRUE,
                             verboseIter = TRUE)
  individualtrain
  model_list1 <- caretList(CompBx ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + 
                             lagMS_AteEnough +lagMS_Macros+lagMS_FoodRule+lagMS_NoneAbove+lagUrgeStrat+
                             lagUrgeStratHelp+lagMood_Strategy + days_from_end , 
                           data= individualtrain, trControl = myControl1,  
                           methodList =c( "rf","nnet","glm")
  )
  modelindividual <- caretEnsemble(model_list1)
  
  #performance on the training set
  table(predict(modelindividual, individualtrain[, -1]), individualtrain$CompBx)
  individualtrain=acc_res(predict(modelindividual, individualtrain[, -1]), individualtrain$CompBx)
  
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
  true.y<- validationset$CompBx
  mm<-ncol(all.ycomb)
  for (i in 1:mm){
    #need to add a few more commands here to select the optimal weight based on classification performance
    temp<-c( acc_res(all.ycomb[,i], true.y) ,i )
    results.wt<-rbind( results.wt,temp)
  }
  
  #specificity, sensitivity, accuracy
  #the weight should be chosen based on this output
  weight.CompBx<-0
  temp<- which(results.wt[,1]>=0.5)
  if(length(temp)>0){
    max.sensi<-max(results.wt[temp,2])
    temp2<-which(results.wt[,2]==max.sensi)
    ind<-which.max(results.wt[temp2,3])
    weight.CompBx<-results.wt[temp2,4][ind] - 1
  } else{
    
    temp3<-which.max(results.wt[,3])
    weight.CompBx<-results.wt[ ,4][temp3] - 1
  }
  # the weight identified
  if(length(weight.CompBx)>1){ 
    weight.CompBx = weight.CompBx[1]
  }
  
  print(weight.CompBx)
  
  #### apply the combined model to the test set
  # the target individual's data in the test set
  individualtest = LOCtest[which( LOCtest$ParticipantID == indx[itarget]), ]
  table(individualtest$CompBx)
  # gropu level prediction  
  yhattest = predict(modelgroup, individualtest[, -1],type="prob")
  # individual level prediction
  yhattest2 = predict(modelindividual, individualtest[, -1],type="prob")
  
  # combine prediction by estimated weight
  weight.CompBx<-as.numeric(weight.CompBx)
  yprobtest=yhattest*weight.CompBx/10+yhattest2*(10-weight.CompBx)/10
  temptest = cbind(individualtest, yprobtest, weight.CompBx,grouptrain[1], grouptrain[2],grouptrain[3],individualtrain[1], individualtrain[2],individualtrain[3])
  testResults = rbind(testResults, temptest)
  
}

#if probability <0.5, Yes
#if probability >=0.5, No
predytest = testResults$yprobtest
predytest[which(testResults$yprobtest >= 0.5)] = "No"
predytest[which(testResults$yprobtest < 0.5)] = "Yes"
#performance on the test set

# Adjust LOCtest to take out low quality participants
LOCtest = LOCtest[-which(LOCtest$ParticipantID%in%IDrm),]
length(predytest)
length(LOCtest$CompBx)

table(predytest, LOCtest$CompBx)
acc_res(predytest, LOCtest$CompBx)
# Speci      Sensi      Acc
# 0.6926287 0.7248322 0.6956522
confusionMatrix(table(predytest, LOCtest$CompBx), positive ="Yes")
#     No Yes
# No  996  41
# Yes 442 108






