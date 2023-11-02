# Group level model for predicting LOC

library(dplyr)
library(ROSE)
library(caret)
library(e1071)
library(caretEnsemble)

getData <- function(data) {
  
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

levels(LOC_All$MS_LOC)=c("No","Yes")

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


#remove subjects with too few LOC cases (<10)
IDrm <- c("2028", "2035","2043", "2063", "2064", "2066","2067", "2068", "2081", "2110", "2122", "2126", "2139")
LOC_All = LOC_All[-which(LOC_All$ParticipantID%in%IDrm),]


IDbyweeks <- LOC_All %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(MS_LOC=="Yes")) ,
            nonLOC = length(which(MS_LOC=="No")) )
View(IDbyweeks)

#training and test sets split, 25% as a test set
indx = unique(LOC_All$ParticipantID)
m = length(indx)
indxTest = NULL
nrow(LOC_All)
for (i in 1:m){
  indx1 = which(LOC_All$ParticipantID == indx[i] &LOC_All$MS_LOC == "Yes")
  indx2 = which(LOC_All$ParticipantID == indx[i] &LOC_All$MS_LOC == "No")
  prop1 = round (length(indx1) * 0.25)
  prop2 = round (length(indx2) * 0.25)
  set.seed(i)
  temp1 = sample(indx1, size = prop1)
  temp2 = sample(indx2, size = prop2)
  indxTest = c(indxTest , temp1, temp2)
}
table(LOC_All$ParticipantID)

train <- LOC_All[-indxTest, ]
test <- LOC_All[indxTest, ]

nrow(train)
table(train$MS_LOC)
#No  Yes 
#7660 1001

nrow(test)
table(test$MS_LOC)
#No  Yes 
#2552  337 

# call the getData() function to prepare the data in an appropriate format
LOCtrain = getData(train)
LOCtest = getData(test)
LOCtest.x = LOCtest[, -1]

# resampling the training set
#LOCtrain.bal<-ovun.sample(Binge ~ . , data=LOCtrain, method="under", p=0.55, seed=5)$data

indx = unique(LOCtrain$ParticipantID)
m = length(indx)
LOCtrain.bal = NULL
nrow(LOCtrain)
table(LOCtrain$Binge)
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


#ensemble learning
set.seed(123)
myControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          search = "grid",
                          savePredictions = "final",
                          index = createResample(LOCtrain.bal$Binge, 10),

                         classProbs = TRUE,
                          verboseIter = TRUE)
nrow(LOCtrain.bal)
nrow(LOCtest.x)
model_list <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                          TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + days_from_end , data=LOCtrain.bal, trControl = myControl,  
 methodList =c( "rf","nnet","glm")
)
model1 <- caretEnsemble(model_list)

#performance on the training set
table(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge)
acc_res(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge)
# specificity sensitiivty accuracy
#0.6663180   0.6813187   0.6739908
confusionMatrix(table(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge), positive ="Yes")
#performance on the test set
table(predict(model1, LOCtest.x), LOCtest$Binge)
#No  Yes
#No  1796  100
#Yes  756  237
acc_res(predict(model1, LOCtest.x), LOCtest$Binge)
# specificity sensitiivty accuracy
# 0.7037618   0.7032641  0.7037037
confusionMatrix(table(predict(model1, LOCtest.x), LOCtest$Binge), positive ="Yes")












