library(dplyr)
library(ROSE)
library(caret)
library(e1071)
library(usethis)
library(dplyr)
library(ROSE)
install.packages("ROSE")
install.packages("dplyr")
install.packages("caretEnsemble")
install.packages("randomForest")
install.packages("usethis")
install.packages("updater")
library(caretEnsemble)
library(randomForest)
library(updater)

urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install.packages(urlPackage, repos=NULL, type="source", dependencies =T) 


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
  indx1 = which(LOC_All$ParticipantID == indx[i] &LOC_All$MS_LOC == "Yes")
  indx2 = which(LOC_All$ParticipantID == indx[i] &LOC_All$MS_LOC == "No")
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
LOCtrain.bal<-ovun.sample(Binge ~ . , data=LOCtrain, method="under", p=0.55, seed=5)$data
table(LOCtrain.bal$Binge)

IDbyweeks_trainbal <- LOCtrain.bal %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(Binge=="Yes")) ,
            nonLOC = length(which(Binge=="No")) )
View(IDbyweeks_trainbal)

glm.fit <- glm( Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                  TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 , data = LOCtrain.bal, family = binomial)
summary (glm.fit)

# training set
glm.probs <- predict (glm.fit, type = "response")
glm.pred <- rep("No", nrow(LOCtrain.bal))
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, LOCtrain.bal$Binge)
acc_res(glm.pred, LOCtrain.bal$Binge)
#0.7144299 0.5998308 0.6520939
#train-test split for each individual
#0.6730310 0.6333666 0.6514410
confusionMatrix(table(glm.pred, LOCtrain.bal$Binge), positive = "Yes")

# test set
glm.probs <- predict(glm.fit, LOCtest.x, type = "response")
glm.pred <- rep("No", nrow(LOCtest.x))
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred, LOCtest$Binge)
acc_res(glm.pred, LOCtest$Binge)
#  0.7635082 0.4935897 0.7479705
#train-test split for each individual
#0.6845611 0.6528190 0.6808584
confusionMatrix(table(glm.pred, LOCtest$Binge), positive ="Yes")

tempp = cbind(LOCtest, glm.pred)
tempp_results <- tempp %>%
  group_by(ParticipantID) %>%
  summarize( nonLOC = table(Binge)[1],
             LOCcases = table(Binge)[2],
             acc = acc_res(glm.pred, Binge)[3],
             sensi = acc_res(glm.pred, Binge)[2],
             speci = acc_res(glm.pred, Binge)[1])
View(tempp_results)



#ensemble learning
myControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          search = "grid",
                          savePredictions = "final",
                          index = createResample(LOCtrain.bal$Binge, 10),
                         # summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                          verboseIter = TRUE)
# myControl <- trainControl(method = "cv", 
#                           number = 10, 
#                           savePredictions = "final", 
#                           classProbs = TRUE)
model_list <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                          TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 , data=LOCtrain.bal, trControl = myControl,  
  #methodList =c( "glm", "glmnet","rf","gbm", "nnet" )
  # cost sensitive classifiers: "rpartCost"
  # https://topepo.github.io/caret/train-models-by-tag.html#cost-sensitive-learning
  #available model in caret
  #https://topepo.github.io/caret/available-models.html
  methodList =c( "rf","nnet")
)
model1 <- caretEnsemble(model_list)
#training set
table(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge)
acc_res(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge)
#0.6326942 0.6996616 0.6691210
confusionMatrix(table(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge), positive ="Yes")
#test set
table(predict(model1, LOCtest.x), LOCtest$Binge)
acc_res(predict(model1, LOCtest.x), LOCtest$Binge)
# 0.6605325 0.5961538 0.6568266
# rf 0.7028191 0.5512821 0.6940959
#rf nnet 0.6887236 0.5961538 0.6833948
confusionMatrix(table(predict(model1, LOCtest.x), LOCtest$Binge), positive ="Yes")

pred_ytest = predict(model1, LOCtest.x)
tempp = cbind(LOCtest, pred_ytest)

tempp_results <- tempp %>%
  group_by(ParticipantID) %>%
  summarize( nonLOC = table(Binge)[1],
             LOCcases = table(Binge)[2],
             acc = acc_res(pred_ytest, Binge)[3],
             sensi = acc_res(pred_ytest, Binge)[2],
             speci = acc_res(pred_ytest, Binge)[1])
View(tempp_results)












set.seed(1)
#SVM linear kernel
tune.out <- tune(svm, Binge ~ ., data = LOCtrain.bal, kernel = "linear", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
bestmod <- tune.out$best.model
summary(bestmod)
#training set
table(bestmod$fitted, LOCtrain.bal$Binge)
length(bestmod$fitted)
length(LOCtrain.bal$Binge)
confusionMatrix(table(bestmod$fitted, LOCtrain.bal$Binge), positive = "Yes")
#test set
pred.test <- predict(bestmod, LOCtest.x)
table(pred.test, LOCtest$Binge)
confusionMatrix(table(pred.test, LOCtest$Binge), positive = "Yes")

#radial kernel
tune.out <- tune(svm, Binge ~ ., data = LOCtrain.bal, kernel = "radial", 
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)
                 )
)
bestmod <- tune.out$best.model
summary(bestmod)
#training set
table(bestmod$fitted, LOCtrain.bal$Binge)
confusionMatrix(table(bestmod$fitted, LOCtrain.bal$Binge), positive = "Yes")
#test set
pred.test <- predict(bestmod, LOCtest.x)
table(pred.test, LOCtest$Binge)
confusionMatrix(table(pred.test, LOCtest$Binge), positive = "Yes")

#polynomial kernel
tune.out <- tune(svm, Binge ~ ., data = LOCtrain.bal, kernel = "polynomial", 
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   degree = c(0.5, 1, 2, 3, 4)
                 )
)


bestmod <- tune.out$best.model
summary(bestmod)
#training set
table(bestmod$fitted, LOCtrain.bal$Binge)
confusionMatrix(table(bestmod$fitted, LOCtrain.bal$Binge), positive = "1")
#test set
pred.test <- predict(bestmod, LOCtest.x)
table(pred.test, LOCtest$Binge)
confusionMatrix(table(pred.test, LOCtest$Binge), positive = "1")




