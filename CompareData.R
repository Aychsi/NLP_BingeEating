LOCTrainTest <- read.csv("/Users/hansoochang/Drexel/CBT+/LOCTrainTest_Complete.csv", 
                         header = TRUE, sep = ",", stringsAsFactors = T)

LOCTrain <- read.csv("LOCTrain_Complete.csv", header = TRUE, sep = ",", stringsAsFactors = T)

head(LOCTrainTest)
head(LOC_All)

merged <- merge(LOCTrainTest, LOC_All, by = "Entry_DateTime") 
head(merged)

aggregate(LOCTrainTest$ParticipantID, by=list(LOCTrainTest$ParticipantID), FUN = length)
aggregate(LOC_All$ParticipantID, by=list(LOC_All$ParticipantID), FUN = length)

LOCTrainTest[LOCTrainTest$ParticipantID == "2010", ]
