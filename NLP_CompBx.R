
LOC_All_NLP_CompBx <- read.csv("/Users/hansoochang/Drexel/CBT+/CBT+ RCT Records_Removed Study Period_Blank Responses_Low Quality_Complete Cases_All Predictors_CompBx.csv", 
                        header = TRUE, sep = ",", stringsAsFactors = T)

head(LOCtrain_CompBx)
colnames(LOCtrain_CompBx)
LOCtrain_CompBx$MS_FoodEaten <- tolower(LOCtrain_CompBx$MS_FoodEaten)
LOCtrain_CompBx$MS_FoodEaten <- sub("halo top", "icecream", LOCtrain_CompBx$MS_FoodEaten)
LOCtrain_CompBx$MS_FoodEaten <- sub("ice cream", "icecream", LOCtrain_CompBx$MS_FoodEaten)
LOCtrain_CompBx$MS_FoodEaten <- sub("protein shake", "proteinshake", LOCtrain_CompBx$MS_FoodEaten)
LOCtrain_CompBx$MS_FoodEaten <- sub("trail mix", "trailmix", LOCtrain_CompBx$MS_FoodEaten)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
system.time( 
  x <- udpipe_annotate(ud_model, x = LOCtrain_CompBx$MS_FoodEaten, doc_id = LOCtrain_CompBx$ParticipantID)
)

x <- as.data.frame(x)
abc <- c("NN")
stats <- dplyr::filter(x,grepl(pattern = paste(abc, collapse = "|"), x = xpos, ignore.case = T))
View(stats)
table(stats$doc_id)

# Words that are not helpful. I might have to remove chicken because it's too common...
stats <- stats[-which(stats$lemma %in% c("pieces", "piece", "slice", "slices", "cups", "cup", "bag", 
                                         "bowl", "bowls", "food")), ]
View(stats[stats$lemma == "vanilla", ])
sorted_food <- sort(table(stats$lemma), decreasing = T)

colnames(stats)
# merged words with original dataframe
a <- merge(LOCtrain_CompBx, stats, by.x = c("ParticipantID", "MS_FoodEaten"), 
           by.y = c("doc_id", "sentence"))

View(a)


# By most common CompBx foods
plot(sort(table(a[a$CompBx == "Yes",]$lemma), decreasing = T)[1:20])
sort(table(a[a$CompBx == "Yes",]$lemma), decreasing = T)[1:20]
top_CompBx_foods <- names(sort(table(a[a$CompBx == "Yes",]$lemma), decreasing = T)[1:20])
top_CompBx_foods
"pizza" %in% top_CompBx_foods

# By highest CompBx to non-CompBx ratio
food_ratio <- as.data.frame(a %>% group_by(lemma) %>% 
                              summarize(CompBxyes = sum(CompBx == "Yes"), CompBxno = sum (CompBx == "No")))
food_ratio[order(-food_ratio$CompBxyes), ]
food_ratio$tot_count <- food_ratio$CompBxyes + food_ratio$CompBxno
food_ratio$ratio_yestono <- food_ratio$CompBxyes / food_ratio$CompBxno

# remove infinites and replace with zero. None of infinites are useful since they are usually 
# one CompBx episode with 0 non CompBx with a particular food
food_ratio[sapply(food_ratio, is.infinite)] <- 0

### it seems as though those with a high ratio have only a few occurrences. We should only include 
# lemmas with 20 occurrences
food_ratio_highqual <- food_ratio[food_ratio$tot_count > 20, ]

food_ratio_highqual[order(-food_ratio_highqual$ratio_yestono, -food_ratio_highqual$tot_count), ]

table(LOCtrain_CompBx$CompBx) # 1:12 CompBx to non CompBx ratio
# Since the average ratio of CompBx to non-CompBx is about 1:12, we will consider only foods with
# a ratio of greater than 0.083 to be "dangerous foods"
View(food_ratio_highqual[food_ratio_highqual$ratio_yestono > 0.083, ])
food_ratio_highqual[food_ratio_highqual$ratio_yestono > 0.083, ]$lemma

top_CompBx_foods <- food_ratio_highqual[food_ratio_highqual$ratio_yestono > 0.083, ]$lemma
top_CompBx_foods




# Place danger_food_CompBx in the regular df
a1 <- a %>% group_by(ParticipantID, Entry_DateTime) %>% summarize(n = sum(danger_food_CompBx))
table(a1$ParticipantID)
table(LOCtrain_CompBx$ParticipantID)

View(a1[a1$ParticipantID == 2010, ])
View(LOCtrain_CompBx[LOCtrain_CompBx$ParticipantID == 2010, ])

a1$n <- ifelse(a1$n > 0, 1, 0) 
View(a1)
table(a1$n)



# It seems as though the top 20 foods that are associated with binging could be useful in the model
cor.test(as.numeric(a$MS_LOC), as.numeric(a$danger_food_CompBx))



