
### Data Cleaning ### 
setwd("/Users/hansoochang/Drexel/CBT+")

library(dplyr)
library(ROSE)
library(caret)
library(e1071)
library(dplyr)
library(ROSE)
library(caretEnsemble)
library(randomForest)
library(fastAdaboost)
library(xgboost)
library(glmnet)
library(stringr)
library(VIM)
library(udpipe)

getDataLong <- function(data) {
  
  #All predictors minus lagMood_Strategy, lagUrgeStratHelp, and lagMS_FoodRule 
  ind <- c("lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx", "lagUrges",  "lagMood", 
           "lagMS_AteEnough", "lagMS_Macros", "lagMS_NoneAbove", "lagUrgeStrat",
           "ParticipantID","days_from_end", "Entry_DateTime", "MS_FoodEaten", "lagMS_FoodRule",
           "lagUrgeStratHelp", "lagMood_Strategy")
  
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

getDataShort <- function(data) {
  
  ind <- c("lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx",
           "lagUrges",  "lagMood", "ParticipantID", "Entry_DateTime", "days_from_end",
           "MS_FoodEaten")
  
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

LOC_All_new <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_All Predictors_LOC.csv", 
                        header = TRUE, sep = ",", stringsAsFactors = T)


LOC_All_new[, "MS_LOC"] <- factor(LOC_All_new[, "MS_LOC"])
LOC_All_new[, "lagMS_Type"] <- factor(LOC_All_new[, "lagMS_Type"])
LOC_All_new[, "lagMS_LOC"] <- factor(LOC_All_new[, "lagMS_LOC"])
LOC_All_new[, "lagCompBx"] <- factor(LOC_All_new[, "lagCompBx"])
LOC_All_new[, "lagUrges"] <- factor(LOC_All_new[, "lagUrges"])

levels(LOC_All_new$MS_LOC)=c("No","Yes")

as.character(LOC_All_new$Entry_DateTime)
LOC_All_new$date_entry_string <- substring(as.character(LOC_All_new$Entry_DateTime), 1, nchar(as.character(LOC_All_new$Entry_DateTime))-5)
LOC_All_new$date_entry_string <- trimws(LOC_All_new$date_entry_string)

LOC_All_new$formatted_entry_date <- as.Date(LOC_All_new$date_entry_string, "%m/%d/%Y")

LOC_All_new <- LOC_All_new %>% 
  group_by(ParticipantID) %>% 
  mutate(enddate = max(formatted_entry_date)) %>%
  ungroup()


LOC_All_new <- as.data.frame(LOC_All_new)

LOC_All_new$days_from_end <- LOC_All_new$formatted_entry_date - LOC_All_new$enddate

colnames(LOC_All)

# Long list of predictors
LOC_All_long <- LOC_All_new[, c("MS_LOC","ParticipantID","days_from_end", "Entry_DateTime", "formatted_entry_date", 
                                "lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx", "lagUrges",  "lagMood", 
                                "lagMS_AteEnough", "lagMS_Macros", "lagMS_NoneAbove", "lagUrgeStrat",
                                "MS_FoodEaten", "lagMS_FoodRule", "lagUrgeStratHelp", "lagMood_Strategy")]



nrow(LOC_All_new) # 13553
nrow(na.omit(LOC_All_long)) # 13551
colSums(is.na(LOC_All_new))


# Remove rows with any NA's in long
naOmit.LOC.long <- na.omit(getDataLong(LOC_All_new))
nrow(na.omit(getDataLong(LOC_All_new))) #13551

table(getDataShort(LOC_All_new)$Binge) # ~1:10 Binge to non-binge ratio
table(getDataShort(LOC_All_new)$ParticipantID)


# Get LOC and non LOC by participant
IDbyweeks1 <- LOC_All_new %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(MS_LOC=="Yes")) ,
            nonLOC = length(which(MS_LOC=="No")) )
View(IDbyweeks1)


#remove subjects with too few LOC cases (<10)
IDrm <- c("2028", "2035","2043", "2057", "2063", "2064", "2066","2067", "2068", "2081", "2110", "2122", "2126", "2139")


LOC_All_new = LOC_All_new[-which(LOC_All_new$ParticipantID%in%IDrm),]

table(LOC_All_new$ParticipantID)
colnames(LOC_All_new)

# Cleaned df Long
df_long <- getDataLong(LOC_All_new)
nrow(LOC_All_new) #11550
nrow(df_long) #11550


#training and test sets split, 25% as a test set (Long)
indx = unique(df_long$ParticipantID)
m = length(indx)
indxTest = NULL
#reschecking = NULL
for (i in 1:m){
  indx1 = which(df_long$ParticipantID == indx[i] &df_long$Binge == "Yes")
  indx2 = which(df_long$ParticipantID == indx[i] &df_long$Binge == "No")
  prop1 = round (length(indx1) * 0.25)
  prop2 = round (length(indx2) * 0.25)
  set.seed(i)
  temp1 = sample(indx1, size = prop1)
  temp2 = sample(indx2, size = prop2)
  indxTest = c(indxTest , temp1, temp2)
}
train_long <- df_long[-indxTest, ]
test_long <- df_long[indxTest, ]

nrow(train_long) # 8661
table(train_long$Binge)
#No  Yes 
#7660  1001 

nrow(test_long) # 2889
table(test_long$Binge)
#No  Yes 
#2552  337

### Find Foods that are predictive of Binge ###
# Make all words lower case and make relevant compound words.
train_long$MS_FoodEaten <- tolower(train_long$MS_FoodEaten)
train_long$MS_FoodEaten <- sub("halo top", "icecream", train_long$MS_FoodEaten)
train_long$MS_FoodEaten <- sub("ice cream", "icecream", train_long$MS_FoodEaten)
train_long$MS_FoodEaten <- sub("protein shake", "proteinshake", train_long$MS_FoodEaten)
train_long$MS_FoodEaten <- sub("trail mix", "trailmix", train_long$MS_FoodEaten)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
system.time( 
  x <- udpipe_annotate(ud_model, x = train_long$MS_FoodEaten, doc_id = train_long$ParticipantID)
)

x <- as.data.frame(x)
# Choose only nouns
abc <- c("NN")
stats <- dplyr::filter(x,grepl(pattern = paste(abc, collapse = "|"), x = xpos, ignore.case = T))
View(stats)
table(stats$doc_id)

# Words that are not helpful. I might have to remove chicken because it's too common...
stats <- stats[-which(stats$lemma %in% c("pieces", "piece", "slice", "slices", "cups", "cup", "bag", 
                                         "bowl", "bowls", "food")), ]

sorted_food <- sort(table(stats$lemma), decreasing = T)


# merged words with original dataframe
a <- merge(train_long, stats, by.x = c("ParticipantID", "MS_FoodEaten"), 
           by.y = c("doc_id", "sentence"))
View(a)


# By highest binge to non-binge ratio
food_ratio <- as.data.frame(a %>% group_by(lemma) %>% 
                              summarize(bingeyes = sum(Binge == "Yes"), bingeno = sum (Binge == "No")))
food_ratio[order(-food_ratio$bingeyes), ]
food_ratio$tot_count <- food_ratio$bingeyes + food_ratio$bingeno
food_ratio$ratio_yestono <- food_ratio$bingeyes / food_ratio$bingeno

# remove infinites and replace with zero. None of infinites are useful since they are usually 
# one binge episode with 0 non binge with a particular food
food_ratio[sapply(food_ratio, is.infinite)] <- 0

### it seems as though those with a high ratio have only a few occurrences. We should only include 
# lemmas with 20 occurrences
food_ratio_highqual <- food_ratio[food_ratio$tot_count > 20, ]

food_ratio_highqual[order(-food_ratio_highqual$ratio_yestono, -food_ratio_highqual$tot_count), ]

# Since the average ratio of binge to non-binge is about 1:7 (0.14), we will consider only foods with
# a ratio of greater than 0.14 to be "dangerous foods"
View(food_ratio_highqual[food_ratio_highqual$ratio_yestono > 0.14, ])
food_ratio_highqual[food_ratio_highqual$ratio_yestono > 0.14, ]$lemma

top_binge_foods <- food_ratio_highqual[food_ratio_highqual$ratio_yestono > 0.14, ]$lemma
# List of top_binge_foods
top_binge_foods


### Add Dangerous Food column (From NLP.R) ###
# Add to training
train_long$danger_food <- 0
for (i in 1:length(train_long$MS_FoodEaten)) {
  for (food in top_binge_foods) {
    if (str_detect(toString(train_long$MS_FoodEaten[i]), food)) {
      train_long$danger_food[i] <- 1
      break
    }
  }
}

# Add to test
test_long$danger_food <- 0
for (i in 1:length(test_long$MS_FoodEaten)) {
  for (food in top_binge_foods) {
    if (str_detect(toString(test_long$MS_FoodEaten[i]), food)) {
      test_long$danger_food[i] <- 1
      break
    }
  }
}
# 1 = MS_FoodEaten includes a dangerous food. Else, 0
train_long$danger_food
test_long$danger_food


# get df with just predictors (Long)
LOCtrain_long = train_long
LOCtest_long = test_long
LOCtrain.x.long <- LOCtrain_long[, !names(LOCtrain_long) %in% c("Binge", "ParticipantID", "Entry_DateTime")]
LOCtest.x.long = LOCtest_long[, !names(LOCtest_long) %in% c("Binge", "ParticipantID", "Entry_DateTime")]


# There seems to be one row with NA
nrow(LOCtest.x.long)
nrow(na.omit(LOCtest.x.long))
LOCtest.x.long <- na.omit(LOCtest.x.long)
LOCtest_long <- na.omit(LOCtest_long)



IDbyweeks_train <- LOCtrain_long %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(Binge=="Yes")) ,
            nonLOC = length(which(Binge=="No")) )
View(IDbyweeks_train)
# resampling the training set (###LONG###)
# The difference between this and the rose resampling is that this loop
# resamples by participant, so that each participant has the same number of
# LOC as non LOC
indx = unique(LOCtrain_long$ParticipantID)
m = length(indx)
LOCtrain.bal.long = NULL
table(LOCtrain_long$Binge)
for (i in 1:m){
  indx1 = which(LOCtrain_long$ParticipantID == indx[i] )
  indx1no = which(LOCtrain_long$ParticipantID == indx[i] 
                  & LOCtrain_long$Binge == "No"   )
  indx1yes = which(LOCtrain_long$ParticipantID == indx[i] 
                   & LOCtrain_long$Binge == "Yes"   )
  # number of non-LOC cases
  m1 = length(indx1no)
  # number of LOC cases
  m2 = length(indx1yes)
  if(m1 > m2) {
    set.seed(i)
    temp1 = sample(indx1no, size = m2)
    indxs = c(temp1, indx1yes)
    LOCtrain.bal.long = rbind(LOCtrain.bal.long, LOCtrain_long[indxs, ])
  } else {
    LOCtrain.bal.long = rbind(LOCtrain.bal.long, LOCtrain_long[indx1, ])
  }
}

table(LOCtrain.bal.long$Binge)

LOCtrain.bal.x.long <- LOCtrain.bal.long[, !names(LOCtrain.bal.long) %in% c("Binge", "ParticipantID", "Entry_DateTime")]

nrow(LOCtrain.bal.long) #1957
table(LOCtrain.bal.long$Binge)

IDbyweeks_trainbal_long <- LOCtrain.bal.long %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(Binge=="Yes")) ,
            nonLOC = length(which(Binge=="No")) )
## By participant, the LOC to nonLOC ratio are not the same. Will need to fix for group + individual
# level models.
View(IDbyweeks_trainbal_long)

nrow(LOCtrain.bal.long) # 1957
nrow(LOCtest.x.long) # 2888

### Modelling with Ensemble Learning ###
set.seed(123)

myControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          search = "grid",
                          savePredictions = "final",
                          index = createResample(LOCtrain.bal.long$Binge, 10),
                          classProbs = TRUE,
                          verboseIter = TRUE)

model_list_long <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                          TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + lagMS_AteEnough + 
                          lagMS_Macros + lagMS_FoodRule + lagMS_NoneAbove + lagUrgeStrat + 
                          lagUrgeStratHelp + lagMood_Strategy + days_from_end + danger_food, data=LOCtrain.bal.long, trControl = myControl,  
                        methodList =c("rf","nnet", "glm")
)


model_list_short <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                          TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + days_from_end + danger_food, data=LOCtrain.bal.long, trControl = myControl,  
                        methodList =c("rf","nnet", "glm")
)



### Long ###
model1 <- caretEnsemble(model_list_long) 
model1$models
summary(resamples(model_list_long))

#performance on the training set
table(predict(model1, LOCtrain.bal.x.long), LOCtrain.bal.long$Binge)
acc_res(predict(model1, LOCtrain.bal.x.long), LOCtrain.bal.long$Binge)
# specificity sensitiivty accuracy
# 0.7583682 0.7392638 0.7487073

confusionMatrix(table(predict(model1, LOCtrain.bal.long[, -1]), LOCtrain.bal.long$Binge), positive ="Yes")
#     No Yes
#No  725 255
#Yes 231 723


#performance on the test set
table(predict(model1, LOCtest.x.long), LOCtest_long$Binge)
#     No  Yes
#No  1911  120
#Yes  641  208
acc_res(predict(model1, LOCtest.x.long), LOCtest_long$Binge)
# Long List (Final)
#
# "rf","nnet", "glm", "xgbLinear"
# 0.6955329 0.7291667 0.6994460
#        No  Yes
#No  1775   91
#Yes  777  245
#
# "rf","nnet", "glm", "xgbLinear", "svmLinear"
# 0.6939655 0.7321429 0.6984072
#        No  Yes
#No  1771   90
#Yes  781  246
#
#"rf","nnet", "glm"
# 0.7002351 0.7321429 0.7039474
#        No  Yes
#No  1787   90
#Yes  765  246
#







### Short List ###
model1 <- caretEnsemble(model_list_short) 
model1$models
summary(resamples(model_list_short))

#performance on the training set
table(predict(model1, LOCtrain.bal.x.short), LOCtrain.bal.short$Binge)
acc_res(predict(model1, LOCtrain.bal.x.short), LOCtrain.bal.short$Binge)

confusionMatrix(table(predict(model1, LOCtrain.bal.short[, -1]), LOCtrain.bal.short$Binge), positive ="Yes")

table(predict(model1, LOCtest.x.short), LOCtest_short$Binge)

acc_res(predict(model1, LOCtest.x.short), LOCtest_short$Binge)

# Short list Test Set results
# "rf","nnet", "glm", "xgbLinear"
# 0.6822100 0.7121662 0.6857044
#        No  Yes
#No  1741   97
#Yes  811  240
#
# "rf","nnet", "glm", "xgbLinear", "svmLinear"
# 0.6869122 0.7091988 0.6895119
#        No  Yes
# No  1753   98
# Yes  799  239
#
# "rf","nnet", "glm"
# 0.6806426 0.7270030 0.6860505
#        No  Yes
#No  1737   92
#Yes  815  245
#
#

# for df with danger_food
nrow(LOCtrain.bal.short) # 1912
nrow(LOCtest_short) # 2873 (Zoe's is 2889)


