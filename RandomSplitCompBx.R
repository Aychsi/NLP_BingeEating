

CompBx_All <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_CompBx.csv", 
                    header = TRUE, sep = ",", stringsAsFactors = T)
CompBx_All_Long <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Incomplete Cases_All Predictors_CompBx.csv", 
                       header = TRUE, sep = ",", stringsAsFactors = T)
CompBx_All_new <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_All Predictors_CompBx.csv", 
                           header = TRUE, sep = ",", stringsAsFactors = T)

ind <- duplicated(CompBx_All_new[, c("ParticipantID", "Entry_DateTime")])
View(CompBx_All_new[ind,])

ind <- duplicated(CompBx_All[, c("ParticipantID", "Entry_DateTime")])
View(CompBx_All[ind,])




