


getDataCompBx <- function(data) {
  
  #predictors  
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
  ind3 <- c("CompBx")
  CompBx<-data[,ind3] 
  CompBx <- as.factor(CompBx)
  
  ##Combine outcome and predictors
  testdata.CompBx<-cbind(CompBx,MLpred)
  
  return(testdata.CompBx)
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

LOC_All_CompBx <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_All Predictors_CompBx.csv", 
                        header = TRUE, sep = ",", stringsAsFactors = T)

LOC_All_CompBx$CompBx <- ifelse(LOC_All_CompBx$CompBx == 1, "Yes", "No") 


LOC_All_CompBx[, "CompBx"] <- factor(LOC_All_CompBx[, "CompBx"])
LOC_All_CompBx[, "lagMS_Type"] <- factor(LOC_All_CompBx[, "lagMS_Type"])
LOC_All_CompBx[, "lagMS_LOC"] <- factor(LOC_All_CompBx[, "lagMS_LOC"])
LOC_All_CompBx[, "lagCompBx"] <- factor(LOC_All_CompBx[, "lagCompBx"])
LOC_All_CompBx[, "lagUrges"] <- factor(LOC_All_CompBx[, "lagUrges"])

levels(LOC_All_CompBx$CompBx)=c("Yes","No")

as.character(LOC_All_CompBx$Entry_DateTime)
LOC_All_CompBx$date_entry_string <- substring(as.character(LOC_All_CompBx$Entry_DateTime), 1, nchar(as.character(LOC_All_CompBx$Entry_DateTime))-5)
LOC_All_CompBx$date_entry_string <- trimws(LOC_All_CompBx$date_entry_string)

LOC_All_CompBx$formatted_entry_date <- as.Date(LOC_All_CompBx$date_entry_string, "%m/%d/%Y")

LOC_All_CompBx <- LOC_All_CompBx %>% 
  group_by(ParticipantID) %>% 
  mutate(enddate = max(formatted_entry_date)) %>%
  ungroup()


LOC_All_CompBx <- as.data.frame(LOC_All_CompBx)

LOC_All_CompBx$days_from_end <- LOC_All_CompBx$formatted_entry_date - LOC_All_CompBx$enddate

LOC_All_CompBx <- LOC_All_CompBx[, c("CompBx","ParticipantID","days_from_end", "Entry_DateTime", "formatted_entry_date", 
                               "lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx", "lagUrges",  "lagMood", 
                               "lagMS_AteEnough", "lagMS_Macros", "lagMS_FoodRule", "lagMS_NoneAbove", "lagUrgeStrat",
                               "lagUrgeStratHelp", "lagMood_Strategy", "MS_FoodEaten")]


# Remove rows with any NA's
naOmit.CompBx <- na.omit(getDataCompBx(LOC_All_CompBx))
nrow(na.omit(getDataCompBx(LOC_All_CompBx))) #14006 There were no NA's after doing getData


IDbyweeks <- getDataCompBx(LOC_All_CompBx) %>%
  group_by(ParticipantID) %>%
  summarize(weeks = length(which(CompBx=="Yes")),
            CompBx = length(which(CompBx=="No")),
            nonCompBx = length(which(CompBx=="Yes")))

View(IDbyweeks)

#remove subjects with too few data points in either CompBx or non-CompBx or overall
IDrm <- c("2013", "2028", "2043", "2045", "2049", "2054", "2057", "2059", "2060", "2063",
          "2065", "2066", "2067", "2068", "2069", "2078", "2080", "2081", "2086", "2108",
          "2122", "2124", "2126", "2135", "2138", "2139")
LOC_All_CompBx = LOC_All_CompBx[-which(LOC_All_CompBx$ParticipantID%in%IDrm),]

#Final cleaned df_CompBx
df_CompBx <- getDataCompBx(LOC_All_CompBx)
nrow(df_CompBx) #7,982
df_CompBx

### Add Dangerous Food column (From NLP_CompBx.R) ###
top_CompBx_foods

# Long df
df_CompBx$danger_food_CompBx <- 0
for (i in 1:length(df_CompBx$MS_FoodEaten)) {
  for (food in top_CompBx_foods) {
    if (str_detect(toString(df_CompBx$MS_FoodEaten[i]), food)) {
      df_CompBx$danger_food_CompBx[i] <- 1
      break
    }
  }
}
df_CompBx$danger_food_CompBx

#training and test sets split, 25% as a test set
indx = unique(df_CompBx$ParticipantID)
m = length(indx)
indxTest = NULL
#reschecking = NULL
for (i in 1:m){
  indx1 = which(df_CompBx$ParticipantID == indx[i] &df_CompBx$CompBx == "Yes")
  indx2 = which(df_CompBx$ParticipantID == indx[i] &df_CompBx$CompBx == "No")
  prop1 = round (length(indx1) * 0.25)
  prop2 = round (length(indx2) * 0.25)
  set.seed(i)
  temp1 = sample(indx1, size = prop1)
  temp2 = sample(indx2, size = prop2)
  indxTest = c(indxTest , temp1, temp2)
}
train_CompBx <- df_CompBx[-indxTest, ]
test_CompBx <- df_CompBx[indxTest, ]

nrow(train_CompBx)
table(train_CompBx$CompBx)
#No  Yes 
#5527  459

nrow(test_CompBx)
table(test_CompBx$CompBx)
#No  Yes 
#1843  153 

# get df_CompBx with just predictors
LOCtrain_CompBx = train_CompBx
LOCtest_CompBx = test_CompBx
LOCtrain.x.CompBx <- LOCtrain_CompBx[, !names(LOCtrain_CompBx) %in% c("CompBx", "ParticipantID", "Entry_DateTime")]
LOCtest.x.CompBx = LOCtest_CompBx[, !names(LOCtest_CompBx) %in% c("CompBx", "ParticipantID", "Entry_DateTime")]

nrow(LOCtrain.x.CompBx) #5986
nrow(LOCtest.x.CompBx) #1996

# There seems to be one row with NA
LOCtest.x.CompBx <- na.omit(LOCtest.x.CompBx)
LOCtest_CompBx <- na.omit(LOCtest_CompBx)

IDbyweeks_train <- LOCtrain_CompBx %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(CompBx=="Yes")) ,
            nonLOC = length(which(CompBx=="No")) )
View(IDbyweeks_train)
# resampling the training set
# The difference between this and the rose resampling is that this loop
# resamples by participant, so that each participant has the same number of
# LOC as non LOC
indx = unique(LOCtrain_CompBx$ParticipantID)
m = length(indx)
LOCtrain.bal.CompBx = NULL
for (i in 1:m){
  indx1 = which(LOCtrain_CompBx$ParticipantID == indx[i] )
  indx1no = which(LOCtrain_CompBx$ParticipantID == indx[i] 
                  & LOCtrain_CompBx$CompBx == "No"   )
  indx1yes = which(LOCtrain_CompBx$ParticipantID == indx[i] 
                   & LOCtrain_CompBx$CompBx == "Yes"   )
  # number of non-LOC cases
  m1 = length(indx1no)
  # number of LOC cases
  m2 = length(indx1yes)
  if(m1 > m2) {
    set.seed(i)
    temp1 = sample(indx1no, size = m2)
    indxs = c(temp1, indx1yes)

    LOCtrain.bal.CompBx = rbind(LOCtrain.bal.CompBx, LOCtrain_CompBx[indxs, ])
  } else {
    LOCtrain.bal.CompBx = rbind(LOCtrain.bal.CompBx, LOCtrain_CompBx[indx1, ])
  }
}

nrow(LOCtrain.bal.CompBx)
table(LOCtrain.bal.CompBx$CompBx)

LOCtrain.bal.x.CompBx <- LOCtrain.bal.CompBx[, !names(LOCtrain.bal.CompBx) %in% c("CompBx", "ParticipantID", "Entry_DateTime")]

nrow(LOCtrain.bal.CompBx) #11083, 889
table(LOCtrain.bal.CompBx$CompBx)

IDbyweeks_trainbal <- LOCtrain.bal.CompBx %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end))/7,
            LOC = length(which(CompBx=="Yes")) ,
            nonLOC = length(which(CompBx=="No")) )
## By participant, the LOC to nonLOC ratio are not the same. Will need to fix for group + individual
# level models.
View(IDbyweeks_trainbal)

nrow(LOCtrain.bal.CompBx)
nrow(LOCtrain_CompBx)
nrow(LOCtest_CompBx)

LOCtrain.bal.CompBx <- na.omit(LOCtrain.bal.CompBx)
LOCtrain.CompBx <- na.omit(LOCtrain_CompBx)

