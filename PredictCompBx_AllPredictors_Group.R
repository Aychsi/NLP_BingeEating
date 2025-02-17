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
library(naivebayes)
# library(Hmisc)

#model_list <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
#                         TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + lagMS_AteEnough + 
##                       lagUrgeStratHelp + lagMood_Strategy + days_from_end.x, data=LOCtrain.bal, trControl = myControl,  
#                    methodList =c( "rf","nnet","glm")

#ensemble learning
set.seed(123)
myControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          search = "grid",
                          savePredictions = "final",
                          index = createResample(LOCtrain.bal.CompBx$CompBx, 10),
                          classProbs = TRUE,
                          verboseIter = TRUE)

model_list_CompBx_long <- caretList(CompBx ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                               TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + lagMS_AteEnough + 
                               lagMS_Macros + lagMS_FoodRule + lagMS_NoneAbove + lagUrgeStrat + 
                               lagUrgeStratHelp + lagMood_Strategy + days_from_end + danger_food_CompBx, data=LOCtrain.bal.CompBx, trControl = myControl,  
                             methodList =c("rf","nnet", "glm")
)


model_list_CompBx_short <- caretList(CompBx ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                                TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 + days_from_end + danger_food_CompBx,
                                data=LOCtrain.bal.CompBx, trControl = myControl,  
                              methodList =c("rf","nnet", "glm")
)

### LONG ###
model1 <- caretEnsemble(model_list_CompBx_long)
model1$models
summary(resamples(model_list_CompBx_long))

#performance on the training set
table(predict(model1, LOCtrain.bal.x.CompBx), LOCtrain.bal.CompBx$CompBx)
acc_res(predict(model1, LOCtrain.bal.x.CompBx), LOCtrain.bal.CompBx$CompBx)

confusionMatrix(table(predict(model1, LOCtrain.bal.CompBx[, -1]), LOCtrain.bal.CompBx$CompBx), positive ="Yes")
#No Yes
#No  329 132
#Yes 101 327
#performance on the test set
table(predict(model1, LOCtest.x.CompBx), LOCtest_CompBx$CompBx)
#No  Yes
#No  1322   40
#Yes  521  113
acc_res(predict(model1, LOCtest.x.CompBx), LOCtest_CompBx$CompBx)
# specificity sensitiivty accuracy
# 0.7173087 0.7385621 0.7189379


confusionMatrix(table(predict(model1, LOCtest.x), LOCtest$CompBx), positive ="Yes")



### SHORT ###
model1 <- caretEnsemble(model_list_CompBx_short)
model1$models
summary(resamples(model_list_CompBx_short))

#performance on the training set
table(predict(model1, LOCtrain.bal.x.CompBx), LOCtrain.bal.CompBx$CompBx)
acc_res(predict(model1, LOCtrain.bal.x.CompBx), LOCtrain.bal.CompBx$CompBx)

confusionMatrix(table(predict(model1, LOCtrain.bal.CompBx[, -1]), LOCtrain.bal.CompBx$CompBx), positive ="Yes")

#performance on the test set
table(predict(model1, LOCtest.x.CompBx), LOCtest_CompBx$CompBx)
#No  Yes
#No  1335   38
#Yes  508  115
acc_res(predict(model1, LOCtest.x.CompBx), LOCtest_CompBx$CompBx)
# specificity sensitivity accuracy
# 0.7243625 0.7516340 0.7264529

nrow(LOCtrain.bal.CompBx) # 889
nrow(LOCtest_CompBx) # 1996

