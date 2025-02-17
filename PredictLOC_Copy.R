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


#ensemble learning
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

# <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
#                       TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + lagMS_AteEnough +
#                        lagMS_Macros + lagMS_NoneAbove + lagUrgeStrat + days_from_end + danger_food,
#                 data=LOCtrain.bal, trControl = myControl,  
#           methodList =c("rf","nnet", "glm", "svmPoly")
#)


### Long ###
model1 <- caretEnsemble(model_list_long) 
model1$models
summary(resamples(model_list_long))

#performance on the training set
table(predict(model1, LOCtrain.bal.x.long), LOCtrain.bal.long$Binge)
acc_res(predict(model1, LOCtrain.bal.x.long), LOCtrain.bal.long$Binge)
# specificity sensitiivty accuracy
# 0.6977300 0.6762246 0.6869773 "rf","nnet", "glm"
# 0.7228196 0.6798088 0.7013142 "rf","nnet", "glm", "xgbTree"
# 0.7670251 0.6941458 0.7305854 "rf","nnet", "xgbTree", "xgbLinear"
confusionMatrix(table(predict(model1, LOCtrain.bal.long[, -1]), LOCtrain.bal.long$Binge), positive ="Yes")
#No Yes
#No  584 271
#Yes 253 566
#performance on the test set
table(predict(model1, LOCtest.x.long), LOCtest_long$Binge)
#      No     Yes
#No  1184   88
#Yes  564  193
acc_res(predict(model1, LOCtest.x.long), LOCtest_long$Binge)
# specificity sensitiivty accuracy with all predictors
# 0.6773455 0.6868327 0.6786594 "rf","nnet", "glm",
# 0.6767735 0.6832740 0.6776737 "rf","nnet", "glm", xgbTree"
# 0.6927918 0.6797153 0.6909808 "rf","nnet", "xgbTree", "xgbLinear"
# 0.6687643 0.6903915 0.6717595 "rf","nnet", "xgbTree"
# 0.6819222 0.6832740 0.6821094 "rf","nnet", "glm", "svmPoly"
# 0.6945080 0.6761566 0.6919665 "rf","nnet", "glm", "svmPoly", "xgbLinear"

# All predictors minus lagMood_Strategy, lagUrgeStratHelp, and lagMS_FoodRule. These were shown to
# be not very good predictors of LOC.
# 0.6727689 0.6939502 0.6757023 "rf","nnet", "glm", "svmPoly", "xgbLinear"
# 0.6733410 0.7046263 0.6776737 "rf","nnet", "glm", "svmPoly"

# All predictors minus lagMood_Strategy, lagUrgeStratHelp, and lagMS_FoodRule with Zoe's Dataframe
# 0.7108150 0.6934524 0.7087950 "rf","nnet", "glm", "svmPoly","xgbLinear"
# 0.7147335 0.6875000 0.7115651 "rf","nnet", "glm"
# 0.7096395 0.6904762 0.7074100 "rf","nnet", "glm", "svmPoly"

# Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
# TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + lagMS_AteEnough +
#  lagMS_Macros + lagMS_NoneAbove + lagUrgeStrat + days_from_end + danger_food
# 0.6880878 0.7227414 0.6919596 "rf", "nnet", "glm", "svmLinear", "xgbLinear"
# 0.6826019 0.7352025 0.6884789 "rf", "nnet", "glm", "svmPoly", "xgbLinear"

# Same as above, but without danger_food
# 0.7025862 0.6728972 0.6992691 "rf","nnet", "glm", "svmLinear", "xgbLinear"

# All predictors plus danger_food and danger_food
# 0.6778997 0.7406250 0.6848886 "rf","nnet", "glm", "svmLinear", "xgbLinear"
# 0.6818182 0.7281250 0.6869777 "rf","nnet", "glm", "svmLinear"
# 0.6806426 0.7437500 0.6876741 "rf","nnet", "glm", "xgbLinear"
#        No  Yes
#No  1738   84
#Yes  814  236
#
#

# All predictors minus days_from_end
# 0.7053292 0.6750000 0.7019499 "rf","nnet", "glm", "xgbLinear"
#        No  Yes
# No  1800  104
# Yes  752  216

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
nrow(LOCtrain.bal.long) # 1912 
nrow(LOCtest_long) # 2873 (Zoe's is 2889)

nrow(LOCtrain.bal.short) # 1912
nrow(LOCtest_short) # 2873 (Zoe's is 2889)


