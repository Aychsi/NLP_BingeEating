# the version of combined model (group level and individual level models are combined using tuned weight)
library(dplyr)
library(ROSE)
library(caret)
library(e1071)
library(caretEnsemble)

getData <- function(data) {
  
  #predictors  
  ind <- c("lagMS_Type", "lagMS_TimeElapsed", "lagMS_LOC", "lagCompBx",
           "lagUrges",  "lagMood", "ParticipantID","days_from_end", "Entry_DateTime")
  
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

View(LOC_All_long[LOC_All_long$ParticipantID == 2039, ])
View(LOC_All_new[LOC_All_new$ParticipantID == 2039, ])

LOC_All <- read.csv("CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_LOC.csv", 
                    header = TRUE, sep = ",", stringsAsFactors = T)

LOC_All_long <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Incomplete Cases_All Predictors_LOC.csv", 
                         header = TRUE, sep = ",", stringsAsFactors = T)

LOC_All_new <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_All Predictors_LOC.csv", 
                        header = TRUE, sep = ",", stringsAsFactors = T)
ncol(LOC_All)
ncol(LOC_All_long)
ncol(LOC_All_new)

nrow(LOC_All) #13553
nrow(LOC_All_long) #19310
nrow(LOC_All_new) #13553

table(LOC_All_new$ParticipantID)
table(LOC_All$ParticipantID)
table(LOC_All_long$ParticipantID)

nrow(LOC_All_new[!complete.cases(LOC_All_new), ])
nrow(LOC_All_new[!complete.cases(LOC_All), ])

ind <- duplicated(LOC_All[, c("ParticipantID", "Entry_DateTime")])
View(LOC_All[ind,])
View(LOC_All[LOC_All$ParticipantID == 2010,])

ind <- duplicated(LOC_All_new[, c("ParticipantID", "Entry_DateTime")])
View(LOC_All_new[ind,])

nrow(merge(x = LOC_All, y = LOC_All_new, by = c("ParticipantID", "Entry_DateTime"), all = F))

# Subjects that are not in both datasets
setdiff(unique(LOC_All_long$ParticipantID), unique(LOC_All$ParticipantID)) # 2049 2077

# remove subjects that are not in both datasets
IDremove <- c("2049", "2077")
LOC_All_long = LOC_All_long[-which(LOC_All_long$ParticipantID%in%IDremove),]

a <- LOC_All[LOC_All$ParticipantID == 2010, ]
b <- LOC_All_long[LOC_All_long$ParticipantID == 2010, ]

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

### Do same (get days_from_end for LOC_All_long)
LOC_All_long[, "MS_LOC"] <- factor(LOC_All_long[, "MS_LOC"])
LOC_All_long[, "lagMS_Type"] <- factor(LOC_All_long[, "lagMS_Type"])
LOC_All_long[, "lagMS_LOC"] <- factor(LOC_All_long[, "lagMS_LOC"])
LOC_All_long[, "lagCompBx"] <- factor(LOC_All_long[, "lagCompBx"])
LOC_All_long[, "lagUrges"] <- factor(LOC_All_long[, "lagUrges"])

levels(LOC_All_long$MS_LOC)=c("No","Yes")

as.character(LOC_All_long$Entry_DateTime)
LOC_All_long$date_entry_string <- substring(as.character(LOC_All_long$Entry_DateTime), 1, nchar(as.character(LOC_All_long$Entry_DateTime))-5)
LOC_All_long$date_entry_string <- trimws(LOC_All_long$date_entry_string)

LOC_All_long$formatted_entry_date <- as.Date(LOC_All_long$date_entry_string, "%m/%d/%Y")

LOC_All_long <- LOC_All_long %>% 
  group_by(ParticipantID) %>% 
  mutate(enddate = max(formatted_entry_date)) %>%
  ungroup()


LOC_All_long <- as.data.frame(LOC_All_long)

LOC_All_long$days_from_end <- LOC_All_long$formatted_entry_date - LOC_All_long$enddate

#remove subjects with too few data points
IDrm <- c("2028", "2035","2043", "2063", "2064", "2066","2067", "2068", "2081", "2110", "2122", "2126", "2139",
          "2013","2016","2025","2045","2054","2057","2059","2086","2099","2108","2019","2123","2135","2136","2138")
LOC_All = LOC_All[-which(LOC_All$ParticipantID%in%IDrm),]

### Remove these same participants from the LOC_All_long
setdiff(unique(LOC_All_long$ParticipantID), unique(LOC_All$ParticipantID))
IDrm_long <- c("2028", "2035","2043", "2063", "2064", "2066","2067", "2068", "2081", "2110", "2122", "2126", "2139",
          "2013","2016","2025","2045","2054","2057","2059","2086","2099","2108","2019","2123","2135","2136","2138")
LOC_All_long = LOC_All_long[-which(LOC_All_long$ParticipantID%in%IDrm_long),]

# Same set of participants with participants with too few data points removed.
table(LOC_All$ParticipantID)
table(LOC_All_long$ParticipantID) # more rows in all participants in this df

# It seems like the extra rows are from rows with many NA's. LOC_All was cleaned of this, but 
# it seems like LOC_All_long has not been.
new_pred <- c("lagMS_AteEnough", "lagMS_Macros", "lagMS_FoodRule", "lagMS_NoneAbove", "lagUrgeStrat",
              "lagUrgeStratHelp", "lagMood_Strategy", "ParticipantID", "days_from_end", 
              "formatted_entry_date", "Entry_DateTime")

# lagMood_StratHelp barely has any datapoints. We should remove it.
sum(is.na(LOC_All_long[, new_pred]$lagMood_StratHelp))
length(LOC_All_long[, new_pred]$lagMood_StratHelp)

# Remove rows with any NA's
naOmit.LOC_long <- na.omit(LOC_All_long[, new_pred])
naOmit.LOC <- na.omit(getData(LOC_All))
nrow(na.omit(LOC_All_long[, new_pred])) #7931
nrow(na.omit(getData(LOC_All))) #8111 There were no NA's after doing getData

# It seems like 2039, in the LOC_All_long df has a big difference in number of rows from
# LOC_All (59 rows vs. 85)
table(naOmit.LOC_long$ParticipantID)
table(naOmit.LOC$ParticipantID)

# Remove 2039
naOmit.LOC_long = naOmit.LOC_long[-which(naOmit.LOC_long$ParticipantID == 2039),]
naOmit.LOC = naOmit.LOC[-which(naOmit.LOC$ParticipantID == 2039),]

# Very close participant numbers.
table(naOmit.LOC_long$ParticipantID)
table(naOmit.LOC$ParticipantID)

naOmit.LOC_long
naOmit.LOC

# Merged df with NA rows removed.
LOC_omit_merged <- merge(naOmit.LOC, naOmit.LOC_long, by.x = c("ParticipantID", "Entry_DateTime"), 
      by.y = c("ParticipantID", "Entry_DateTime"), all.x = T, all.y = T)

# See if there are any NA's after merging
nrow(LOC_omit_merged[rowSums(is.na(LOC_omit_merged)) > 0, ]) #153 rows with NA

# These are the participants who are in LOC_All but not LOC_All_long it seems.
LOC_omit_merged[rowSums(is.na(LOC_omit_merged)) > 0, ]

LOC_omit_merged <- na.omit(LOC_omit_merged)

nrow(LOC_omit_merged) # Final dataset is 7893
#training and test sets split, 25% as a test set
indx = unique(LOC_omit_merged$ParticipantID)
m = length(indx)
indxTest = NULL
#reschecking = NULL
for (i in 1:m){
  indx1 = which(LOC_omit_merged$ParticipantID == indx[i] &LOC_omit_merged$Binge == "Yes")
  indx2 = which(LOC_omit_merged$ParticipantID == indx[i] &LOC_omit_merged$Binge == "No")
  prop1 = round (length(indx1) * 0.25)
  prop2 = round (length(indx2) * 0.25)
  set.seed(i)
  temp1 = sample(indx1, size = prop1)
  temp2 = sample(indx2, size = prop2)
  indxTest = c(indxTest , temp1, temp2)
}
train <- LOC_omit_merged[-indxTest, ]
test <- LOC_omit_merged[indxTest, ]

nrow(train)
table(train$Binge)
#No  Yes 
#5098 821

nrow(test)
table(test$Binge)
#No  Yes 
#1699  275 

# call the getData() function to prepare the data in an appropriate format
LOCtrain = train
LOCtest = test
LOCtrain.x <- LOCtrain[, -c(1,2,3,24,25)]
LOCtest.x = LOCtest[, -c(1,2,3,24,25)]
IDbyweeks_train <- LOCtrain %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end.x))/7,
            LOC = length(which(Binge=="Yes")) ,
            nonLOC = length(which(Binge=="No")) )

# resampling the training set
# The difference between this and the rose resampling is that this loop
# resamples by participant, so that each participant has the same number of
# LOC as non LOC
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

LOCtrain.bal.x <- LOCtrain.bal[, -c(1,2,3,24,25)]

nrow(LOCtrain.bal) #1642
table(LOCtrain.bal$Binge)

IDbyweeks_trainbal <- LOCtrain.bal %>%
  group_by(ParticipantID) %>%
  summarize(weeks = max(abs(days_from_end.x))/7,
            LOC = length(which(Binge=="Yes")) ,
            nonLOC = length(which(Binge=="No")) )
## By participant, the LOC to nonLOC ratio are not the same. Will need to fix for group + individual
# level models.
View(IDbyweeks_trainbal)
































# Helper function for balanceData function #
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
  Binge <- data[,ind3] 
  Binge <- as.factor(Binge)
  
  ##Combine outcome and predictors
  testtrain.binge<-cbind(Binge,MLpred)
  
  return(testtrain.binge)
}

# Function used to balance the data. Data is the data from getData(), seed is the random seed,
# p is the proportion of the split in the resampling function. 
balanceData <- function(data, seed, p) {
  data1.imp.bal<-ovun.sample(Binge ~ ., data=data, method="both", p=p, seed=seed)$data
  return(data1.imp.bal)
}


### Example ### 

# All Data
LOC_All <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_LOC.csv", 
                    header = TRUE, sep = ",", stringsAsFactors = T)
LOC_All[, "MS_LOC"] <- factor(LOC_All[, "MS_LOC"])
LOC_All[, "lagMS_Type"] <- factor(LOC_All[, "lagMS_Type"])
LOC_All[, "lagMS_LOC"] <- factor(LOC_All[, "lagMS_LOC"])
LOC_All[, "lagCompBx"] <- factor(LOC_All[, "lagCompBx"])
LOC_All[, "lagUrges"] <- factor(LOC_All[, "lagUrges"])
LOC_All[, "lagMood"] <- factor(LOC_All[, "lagMood"])
LOC_All[, "lagMS_TimeElapsed"] <- factor(LOC_All[, "lagMS_TimeElapsed"])

attach(LOC_All)

# Split 75-25
ind <- is.na(MS_LOC)
data <- LOC_All[!ind, ]

# parsedData <- getData(data)
parsedData <- data
set.seed(1)
n=nrow(parsedData)
ranTrain = sample(1:n, 3*n/4) # 75% of all data
ranTest = c(1:n)[-ranTrain] # 25% of all data

ranTrain <- data[ranTrain,] 
ranTest <- data[ranTest,]

nrow(ranTrain)
nrow(ranTest)

table(ranTrain$MS_LOC)
table(ranTest$MS_LOC)


