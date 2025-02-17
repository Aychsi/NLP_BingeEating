install.packages("data.table")
install.packages("mltools")
library(data.table)
library(mltools)

### One hot encode data
dummy <- dummyVars("~.", data = LOCtrain.bal.x)
dummy
newdata <- data.frame(predict(dummy, newdata = LOCtrain.bal.x))

grid_tune <- expand.grid(
  nrounds = c(100, 500, 1000), #number of trees
  max_depth = c(2,4,6),
  eta = 0.05, #c(0.025,0.05,0.1,0.3), #Learning rate
  gamma = 0, # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 0.8,
  min_child_weight = 0.5, # the larger, the more conservative the model
  #is; can be used as a stop
  subsample = 1# used to prevent overfitting by sampling X% training
)

train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)


xgb_tune <- train(x = LOCtrain.bal.x,
                  y = LOCtrain.bal[,1],
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  verbose = TRUE
)
xgb_tune

xgb_tune$bestTune
#     nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 261    1000         2 0.05     0              0.8                1         1

# Writing out the best model.

train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)
xgb_model <- train(x = data_train_x[, -18],
                   y = as.factor(data_train$V2),
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)

data_train_bal.x <- data_train_bal[, -c(28,27,26)]
data_train[, -18]
xgb.pred.train <- predict(xgb_model, data_train[, -18])
as.factor(data_train$V2)
as.factor(as.numeric(xgb.pred.train))
xgb.pred.train
confusionMatrix((xgb.pred.train),
                as.factor(data_train$V2), positive = "1")

# Prediction:

xgb.pred <- predict(xgb_model, data_test_x[,-18])
# Confusion Matrix
confusionMatrix(as.factor(as.numeric(xgb.pred)),
                as.factor(as.numeric(LOCtest$Binge)), positive = "1")

table(data_train$Binge)
table(data_test$Binge)

#ensemble learning
myControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          search = "grid",
                          savePredictions = "final",
                          index = createResample(LOCtrain.bal$Binge, 10),
                          # summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          verboseIter = TRUE)
# myControl <- trainControl(method = "cv", 
#                           number = 10, 
#                           savePredictions = "final", 
#                           classProbs = TRUE)
model_list <- caretList(Binge ~ lagMS_TimeElapsed + lagMS_LOC + lagCompBx + lagUrges + lagMood + TimeCat1 + TimeCat2 +
                          TimeCat3 + TimeCat4 + TimeCat5 + TimeCat6 + TimeCat7 , data=LOCtrain.bal, trControl = myControl,  
                        #methodList =c( "glm", "glmnet","rf","gbm", "nnet" )
                        # cost sensitive classifiers: "rpartCost"
                        # https://topepo.github.io/caret/train-models-by-tag.html#cost-sensitive-learning
                        #available model in caret
                        #https://topepo.github.io/caret/available-models.html
                        methodList =c( "rf","nnet")
)
model1 <- caretEnsemble(model_list)
#training set
table(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge)
acc_res(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge)
#0.6326942 0.6996616 0.6691210
confusionMatrix(table(predict(model1, LOCtrain.bal[, -1]), LOCtrain.bal$Binge), positive ="Yes")
#test set
table(predict(model1, LOCtest.x), LOCtest$Binge)
acc_res(predict(model1, LOCtest.x), LOCtest$Binge)
# 0.6605325 0.5961538 0.6568266
# rf 0.7028191 0.5512821 0.6940959
#rf nnet 0.6887236 0.5961538 0.6833948
confusionMatrix(table(predict(model1, LOCtest.x), LOCtest$Binge), positive ="Yes")

pred_ytest = predict(model1, LOCtest.x)
tempp = cbind(LOCtest, pred_ytest)

tempp_results <- tempp %>%
  group_by(ParticipantID) %>%
  summarize( nonLOC = table(Binge)[1],
             LOCcases = table(Binge)[2],
             acc = acc_res(pred_ytest, Binge)[3],
             sensi = acc_res(pred_ytest, Binge)[2],
             speci = acc_res(pred_ytest, Binge)[1])
View(tempp_results)












set.seed(1)
#SVM linear kernel
tune.out <- tune(svm, Binge ~ ., data = LOCtrain.bal, kernel = "linear", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
bestmod <- tune.out$best.model
summary(bestmod)
#training set
table(bestmod$fitted, LOCtrain.bal$Binge)
length(bestmod$fitted)
length(LOCtrain.bal$Binge)
confusionMatrix(table(bestmod$fitted, LOCtrain.bal$Binge), positive = "Yes")
#test set
pred.test <- predict(bestmod, LOCtest.x)
table(pred.test, LOCtest$Binge)
confusionMatrix(table(pred.test, LOCtest$Binge), positive = "Yes")

#radial kernel
tune.out <- tune(svm, Binge ~ ., data = LOCtrain.bal, kernel = "radial", 
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)
                 )
)
bestmod <- tune.out$best.model
summary(bestmod)
#training set
table(bestmod$fitted, LOCtrain.bal$Binge)
confusionMatrix(table(bestmod$fitted, LOCtrain.bal$Binge), positive = "Yes")
#test set
pred.test <- predict(bestmod, LOCtest.x)
table(pred.test, LOCtest$Binge)
confusionMatrix(table(pred.test, LOCtest$Binge), positive = "Yes")

#polynomial kernel
tune.out <- tune(svm, Binge ~ ., data = LOCtrain.bal, kernel = "polynomial", 
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   degree = c(0.5, 1, 2, 3, 4)
                 )
)


bestmod <- tune.out$best.model
summary(bestmod)
#training set
table(bestmod$fitted, LOCtrain.bal$Binge)
confusionMatrix(table(bestmod$fitted, LOCtrain.bal$Binge), positive = "1")
#test set
pred.test <- predict(bestmod, LOCtest.x)
table(pred.test, LOCtest$Binge)
confusionMatrix(table(pred.test, LOCtest$Binge), positive = "1")




