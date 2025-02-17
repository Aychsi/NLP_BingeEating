
if (!require("dplyr")) {
  install.packages("dplyr", repos="http://cran.rstudio.com/") 
  library("dplyr")
}

LOC_All <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_LOC.csv", 
                    header = TRUE, sep = ",", stringsAsFactors = T)
LOC_All$MS_FoodEaten
head(LOC_All)

LOC_All[, "MS_LOC"] <- factor(LOC_All[, "MS_LOC"])
LOC_All[, "lagMS_Type"] <- factor(LOC_All[, "lagMS_Type"])
LOC_All[, "lagMS_LOC"] <- factor(LOC_All[, "lagMS_LOC"])
LOC_All[, "lagCompBx"] <- factor(LOC_All[, "lagCompBx"])
LOC_All[, "lagUrges"] <- factor(LOC_All[, "lagUrges"])


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

train <- LOC_All[abs(LOC_All$days_from_end) > 14, ]
test <- LOC_All[abs(LOC_All$days_from_end) <= 14, ]

table(LOC_All$ParticipantID)

### Get Ratios ###
nrow(train)
nrow(test)

range(LOC_All$days_from_end)

firstTwoWeeks <- LOC_All[(abs(LOC_All$days_from_end) >= 169) & 
          (abs(LOC_All$days_from_end) < 183), ]
threeFourWeeks <- LOC_All[(abs(LOC_All$days_from_end) >= 155) & 
                           (abs(LOC_All$days_from_end) < 169), ]
fiveSixWeeks <- LOC_All[(abs(LOC_All$days_from_end) >= 141) & 
                            (abs(LOC_All$days_from_end) < 155), ]
sevenEightWeeks <- LOC_All[(abs(LOC_All$days_from_end) >= 127) & 
                          (abs(LOC_All$days_from_end) < 141), ]

nrow(firstTwoWeeks)
nrow(threeFourWeeks)
nrow(fiveSixWeeks)
nrow(sevenEightWeeks)
nrow(test)

(table(firstTwoWeeks$MS_LOC)[1] + table(firstTwoWeeks$MS_LOC)[2]) / 
  table(firstTwoWeeks$MS_LOC)[2]

(table(threeFourWeeks$MS_LOC)[1] + table(threeFourWeeks$MS_LOC)[2]) / 
  table(threeFourWeeks$MS_LOC)[2]

(table(fiveSixWeeks$MS_LOC)[1] + table(fiveSixWeeks$MS_LOC)[2]) / 
  table(fiveSixWeeks$MS_LOC)[2]

(table(sevenEightWeeks$MS_LOC)[1] + table(sevenEightWeeks$MS_LOC)[2]) / 
  table(sevenEightWeeks$MS_LOC)[2]

(table(train$MS_LOC)[1] + table(train$MS_LOC)[2]) / 
  table(train$MS_LOC)[2]

(table(test$MS_LOC)[1] + table(test$MS_LOC)[2]) / 
  table(test$MS_LOC)[2]

(table(ChristTrain$y)[1] + table(ChristTrain$y)[2]) / 
  table(ChristTrain$y)[2]

(table(ChristTest$y)[1] + table(ChristTest$y)[2]) / 
  table(ChristTest$y)[2]

nrow(LOC_All)
prop <- round(3*nrow(LOC_All) / 4)
prop
picked <- sample(seq_len(nrow(LOC_All)), size = prop)
ranTrain <- LOC_All[picked, ]
ranTest <- LOC_All[-picked, ]
nrow(ranTrain)
nrow(ranTest)
