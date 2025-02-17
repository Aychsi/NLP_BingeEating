splits <- c(14, 21, 28, 35)
ps <- c(.40, .45, .50, .55, .60)

for (split in splits) {
  # Training Set #
  train <- LOC_All[abs(LOC_All$num_days_from_end) > split,]
  
  # Test Set #
  test <- LOC_All[abs(LOC_All$num_days_from_end) <= split,]
  
  for (p in ps) {
    trainingMatrix(44, p)
    testMatrix(44, p)
  }
}


LOCData <- read.csv("LOCTrainTest_Complete.csv", header = TRUE, sep = ",", stringsAsFactors = T)
attach(LOCData)

train <- (SetType < 2563)

LOCData.Test <- LOCData[!train, ]

MS_LOC.Test <- MS_LOC[!train]

table(LOCData.Test$MS_LOC)
table(test$MS_LOC)


Yes_LDT <- LOCData.Test[LOCData.Test$MS_LOC == 1, ]
Yes_test <- test[test$MS_LOC == 1, ]

nrow(Yes_LDT)
nrow(Yes_test)

nrow(merge(Yes_LDT, Yes_test, by = c("ParticipantID", "Entry_DateTime")))

merged <- merge(LOCData.Test, test, by = c("ParticipantID", "Entry_DateTime"))

# These should be all the participants who had less than 14 days of data.
question <- merged[abs(merged$num_days_from_end) < 14, ]

nrow(LOC_All[LOC_All$Number_of_days_in_study < 14, ])

table(LOC_All$ParticipantID)
table(LOCData.Test$ParticipantID)
table(test$ParticipantID)

anti <- anti_join(LOCData.Test, test, by = "Entry_DateTime")

a <- test[test$ParticipantID == 2025, ]

b <- LOC_All[LOC_All$ParticipantID == 2025, ]

c <- LOCData.Test[LOCData.Test$ParticipantID == 2025, ]

typeof(test$Entry_DateTime)



