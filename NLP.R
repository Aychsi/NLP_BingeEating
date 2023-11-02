install.packages("udpipe")
library(udpipe)

LOC_All_NLP <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_All Predictors_LOC.csv", 
                        header = TRUE, sep = ",", stringsAsFactors = T)

head(train)
colnames(train)
train$MS_FoodEaten <- tolower(train$MS_FoodEaten)
train$MS_FoodEaten <- sub("halo top", "icecream", train$MS_FoodEaten)
train$MS_FoodEaten <- sub("ice cream", "icecream", train$MS_FoodEaten)
train$MS_FoodEaten <- sub("protein shake", "proteinshake", train$MS_FoodEaten)
train$MS_FoodEaten <- sub("trail mix", "trailmix", train$MS_FoodEaten)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
system.time( 
  x <- udpipe_annotate(ud_model, x = train$MS_FoodEaten, doc_id = train$ParticipantID)
)

x <- as.data.frame(x)
abc <- c("NN")
stats <- dplyr::filter(x,grepl(pattern = paste(abc, collapse = "|"), x = xpos, ignore.case = T))
View(stats)
table(stats$doc_id)

# Words that are not helpful. I might have to remove chicken because it's too common...
stats <- stats[-which(stats$lemma %in% c("pieces", "piece", "slice", "slices", "cups", "cup", "bag", 
                        "bowl", "bowls", "food")), ]

sorted_food <- sort(table(stats$lemma), decreasing = T)

colnames(stats)
# merged words with original dataframe
a <- merge(train, stats, by.x = c("ParticipantID", "MS_FoodEaten"), 
      by.y = c("doc_id", "sentence"))

View(train)
View(stats)
View(a)


# By most common binge foods
plot(sort(table(a[a$Binge == "Yes",]$lemma), decreasing = T)[1:20])
sort(table(a[a$Binge == "Yes",]$lemma), decreasing = T)[1:20]
top_binge_foods <- names(sort(table(a[a$Binge == "Yes",]$lemma), decreasing = T)[1:20])
top_binge_foods
"pizza" %in% top_binge_foods

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
top_binge_foods







