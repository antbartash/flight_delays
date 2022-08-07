### load libraries and data
### decode y labels (to make functions work properly)

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(MLmetrics)
library(ROCR)

setwd('C:\\Users\\olial\\OneDrive\\Desktop\\Desktop\\Flights\\data')

flights <- read_csv('data_final.csv', na = "")
flights$DepDel15 <- ifelse(flights$DepDel15 == 1, 'Delayed', 'Not_Delayed')
flights$DepDel15 <- factor(flights$DepDel15)

set.seed(0)


### split the data into train and test sets
inTraining <- createDataPartition(flights$DepDel15, p = .8, list = FALSE)
flights_train <- flights[inTraining, ]
flights_test <- flights[-inTraining, ]


#### TREE_DEFAULT ####
### build and evaluate a baseline model
tree_default <- rpart(DepDel15 ~., flights_train)
rpart.plot(tree_default, tweak = 1.2)
predicted_default <- predict(tree_default, newdata = flights_test, type = 'class')
confusionMatrix(predicted_default, flights_test$DepDel15, positive = 'Delayed')

### to get deeper trees it's possible to undersample data (increasing a priori 
### probability of positive class), but for model selection AUC will be used, 
### so a priori probability doesn't matter



#### CP TUNING ####
### lower the complexity parameter to get deeper trees
### tune cp using grid search with CV
fitControl <- trainControl(method = 'cv', number = 5,
                           summaryFunction = twoClassSummary,
                           verboseIter = TRUE, classProbs = TRUE)
rpartGrid <- expand.grid(cp = seq(0.00001, 0.002, by = 0.00001))

# takes about 6-9 minutes
tree_cv <- train(DepDel15 ~ ., data = flights_train, 
                 method = "rpart", 
                 trControl = fitControl,
                 tuneGrid = rpartGrid)
View(tree_cv$results)

cv_results <- tree_cv$results
cv_results[which.max(cv_results$Sens), ]    ### cp = 1e-5 maximizes TPR
cv_results[which.max(cv_results$ROC), ]     ### cp = 9e-5 maximizes AUC


plot(tree_cv, ylab = "AUC (Cross-Validation)")


# make sure that greater cp values doesn't improve model's quality
# takes about 3-4 minutes
rpartGrid2 <- expand.grid(cp = seq(0.002, 0.01, by = 0.0001))
tree_cv2 <- train(DepDel15 ~ ., data = flights_train, 
                  method = "rpart", 
                  trControl = fitControl,
                  tuneGrid = rpartGrid2)
Sys.time() - start_time
plot(tree_cv2, ylab = "AUC (Cross-Validation)")



#### MODELS COMPARISON ####
tree_auc <- rpart(DepDel15 ~., flights_train,
                  control = rpart.control(cp = 0.00009)) 
predicted_tree_auc <- predict(tree_auc, newdata = flights_test, type = 'class')
confusionMatrix(predicted_tree_auc, flights_test$DepDel15, positive = 'Delayed')


tree_sens <- rpart(DepDel15 ~., flights_train,
                   control = rpart.control(cp = 1e-5)) 
predicted_tree_sens <- predict(tree_sens, newdata = flights_test, type = 'class')
confusionMatrix(predicted_tree_sens, flights_test$DepDel15)


### low cp leads to trees which are hard to interpret --> set maxdepth=5
tree_int <- rpart(DepDel15 ~., flights_train,
                  control = rpart.control(cp = 1e-5, maxdepth = 5))
predicted_tree_int <- predict(tree_int, newdata = flights_test, type = 'class')
confusionMatrix(predicted_tree_int, flights_test$DepDel15, positive = 'Delayed')

### function to split labels into multiple lines in a tree visualization
split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 28), collapse = "\n")
  }
  labs
}

rpart.plot(tree_int, cex = 0.78,
           split.fun = split.fun)



### plot trees' ROCs
cont_prediction_default <- predict(tree_default, newdata = flights_test, type = 'prob')
cont_prediction_default <- as.vector(cont_prediction_default[, 2])
plot(performance(prediction(cont_prediction_default, flights_test$DepDel15),
                 "tpr", "fpr"),
     lwd = 3, col = "#ff77ff")

cont_prediction_auc <- predict(tree_auc, newdata = flights_test, type = 'prob')
cont_prediction_auc <- as.vector(cont_prediction_auc[, 2])
plot(performance(prediction(cont_prediction_auc, flights_test$DepDel15),
                 "tpr", "fpr"),
     lwd = 3, col = '#006400') 

cont_prediction_sens <- predict(tree_sens, newdata = flights_test, type = 'prob')
cont_prediction_sens <- as.vector(cont_prediction_sens[, 2])
plot(performance(prediction(cont_prediction_sens, flights_test$DepDel15),
                 'tpr', 'fpr'),
     lwd = 3, col = '#ff9f00')

cont_prediction_int <- predict(tree_int, newdata = flights_test, type = 'prob')
cont_prediction_int <- as.vector(cont_prediction_int[, 2])
plot(performance(prediction(cont_prediction_int, flights_test$DepDel15),
                 'tpr', 'fpr'),
     lwd = 3, col = '#4169e1')



### ROCs on one plot
plot(performance(prediction(cont_prediction_default, flights_test$DepDel15),
                 "tpr", "fpr")@x.values[[1]],
     performance(prediction(cont_prediction_default, flights_test$DepDel15),
                 "tpr", "fpr")@y.values[[1]],
     type = 'l', lwd = 3, col = '#ff77ff',
     xlab = 'False positive rate',
     ylab = 'True positive rate')

lines(performance(prediction(cont_prediction_auc, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_auc, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#006400')

lines(performance(prediction(cont_prediction_sens, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_sens, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#ff9f00')

lines(performance(prediction(cont_prediction_int, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_int, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#4169e1')

legend('bottomright', 
       legend = c('tree_default', 'tree_auc', 'tree_sens', 'tree_int'),
       col = c('#ff77ff', '#006400', '#ff9f00', '#4169e1'),
       lty = 1,
       cex = 1.15)



### AUC

perf_auc_default <- performance(prediction(cont_prediction_default, flights_test$DepDel15),
                                "auc")
perf_auc_default@y.values[[1]]

perf_auc_auc <- performance(prediction(cont_prediction_auc, flights_test$DepDel15),
                            "auc")
perf_auc_auc@y.values[[1]]

perf_auc_sens <- performance(prediction(cont_prediction_sens, flights_test$DepDel15),"auc")
perf_auc_sens@y.values[[1]]

perf_auc_int <- performance(prediction(cont_prediction_int, flights_test$DepDel15),"auc")
perf_auc_int@y.values[[1]]



### for TPR >= 0.6 all trees (excl. tree_default) have about the same quality
### for TPR < 0.6 the best quality is achieved with tree_auc model
### tree_auc also has the highest AUC, but the tree is very deep
### tree_int model is easier to interpret
