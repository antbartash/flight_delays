### load libraries and data
### change variables types

library(tidyverse)
library(rpart)
library(randomForest)
library(ada)
library(caret)
library(MLmetrics)
library(ROCR)


setwd('C:\\Users\\olial\\OneDrive\\Desktop\\Desktop\\Flights\\data')

flights <- read_csv('data_final.csv', na = "")
flights$DepDel15 <- as.character(flights$DepDel15)

flights <- flights %>% mutate(
  Reporting_Airline = as.factor(Reporting_Airline),
  origin_grouped  = as.factor(origin_grouped),
  Distance_grouped = as.factor(Distance_grouped),
  mfr_grouped = as.factor(Distance_grouped),
  model_grouped = as.factor(model_grouped),
  n_seats_grouped = as.factor(n_seats_grouped),
  origin_tmpf_grouped = as.factor(origin_tmpf_grouped),
  origin_dwpf_grouped = as.factor(origin_dwpf_grouped),
  origin_relh_grouped = as.factor(origin_relh_grouped),
  origin_vsby_grouped = as.factor(origin_vsby_grouped),
  month = as.factor(month),
  weekday = as.factor(weekday),
  hour_grouped = as.factor(hour_grouped),
  Holiday2 = as.factor(Holiday2)
)

set.seed(0)


### split the data into train, validation and test sets
inTraining <- createDataPartition(flights$DepDel15, p = .8, list = FALSE)
flights_train <- flights[inTraining, ]
flights_test <- flights[-inTraining, ]

inTraining2 <- createDataPartition(flights_train$DepDel15, p = .8, list = FALSE)
flights_valid <- flights_train[-inTraining2, ]
flights_train2 <- flights_train[inTraining2, ]



#### TREES ####

### tree_auc model
tree_auc <- rpart(DepDel15 ~., flights_train2,
                  control = rpart.control(cp = 0.00009))
predicted_tree_auc <- predict(tree_auc, newdata = flights_test, type = 'class')
confusionMatrix(as.factor(predicted_tree_auc), as.factor(flights_test$DepDel15), 
                positive = '1')

cont_prediction_tree_auc <- predict(tree_auc, newdata = flights_test, type = 'prob')
cont_prediction_tree_auc <- as.vector(cont_prediction_tree_auc[, 2])
performance(prediction(cont_prediction_tree_auc, flights_test$DepDel15),
            "auc")@y.values[[1]]
### AUC is about # 0.669132

# tree_int model
tree_int <- rpart(DepDel15 ~., flights_train2,
                  control = rpart.control(cp = 1e-5, maxdepth = 5))
predicted_tree_int <- predict(tree_int, newdata = flights_test, type = 'class')
confusionMatrix(as.factor(predicted_tree_int), as.factor(flights_test$DepDel15))

cont_prediction_tree_int <- predict(tree_int, newdata = flights_test, type = 'prob')
cont_prediction_tree_int <- as.vector(cont_prediction_tree_int[, 2])
performance(prediction(cont_prediction_tree_int, flights_test$DepDel15),
            "auc")@y.values[[1]]
### AUC is about 0.6432958


### save predictions as a file
tree_auc_pred <- prediction(cont_prediction_tree_auc, flights_test$DepDel15)
tree_int_pred <- prediction(cont_prediction_tree_int, flights_test$DepDel15)
# saveRDS(tree_auc_pred, 'tree_auc_pred.rds')
# saveRDS(tree_int_pred, 'tree_int_pred.rds')


### plot ROCs
plot(performance(prediction(cont_prediction_tree_auc, flights_test$DepDel15), 
                 "tpr", "fpr"),
     lwd = 3, col = '#ffdf00')

lines(performance(prediction(cont_prediction_tree_int, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_tree_int, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#4169e1')



#### FOREST ####

### reload the data
### decode the target variable to make functions work properly
flights <- read_csv('data_final.csv', na = "")
flights$DepDel15 <- ifelse(flights$DepDel15 == 1, 'Delayed', 'Not_Delayed')
flights$DepDel15 <- as.factor(flights$DepDel15)


### split the data into train, validation and test sets
inTraining <- createDataPartition(flights$DepDel15, p = .8, list = FALSE)
flights_train <- flights[inTraining, ]
flights_test <- flights[-inTraining, ]

inTraining2 <- createDataPartition(flights_train$DepDel15, p = .8, list = FALSE)
flights_valid <- flights_train[-inTraining2, ]
flights_train2 <- flights_train[inTraining2, ]


### build and evaluate the forest model
forest <- randomForest(DepDel15~., flights_train2, 
                       mtry = 3, ntree = 500)
predicted_forest <- predict(forest, newdata = flights_test, type = 'class')
confusionMatrix(predicted_forest, flights_test$DepDel15)

cont_prediction_rf <- predict(forest, newdata = flights_test, type = 'prob')
cont_prediction_rf <- as.vector(cont_prediction_rf[, 2])
performance(prediction(cont_prediction_rf, flights_test$DepDel15),
            "auc")@y.values[[1]] 
### AUC is about 0.7124086


### save predictions as a file
forest_pred <- prediction(cont_prediction_rf, flights_test$DepDel15)
# saveRDS(forest_pred, 'forest_pred.rds')


### plot ROC
lines(performance(prediction(cont_prediction_rf, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_rf, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#228b22')




#### ADABOOST ####

### reload the data
### decode the target variable to make functions work properly
flights <- read_csv('data_final.csv', na = "")
flights$DepDel15 <- ifelse(flights$DepDel15 == 1, 'Delayed', 'Not_Delayed')
flights$DepDel15 <- as.factor(flights$DepDel15)


### split the data into train, validation and test sets
inTraining <- createDataPartition(flights$DepDel15, p = .8, list = FALSE)
flights_train <- flights[inTraining, ]
flights_test <- flights[-inTraining, ]

inTraining2 <- createDataPartition(flights_train$DepDel15, p = .8, list = FALSE)
flights_valid <- flights_train[-inTraining2, ]
flights_train2 <- flights_train[inTraining2, ]


# build the AdaBoost model
ada_001 <- ada(DepDel15~., data = flights_train2,
               iter = 2000, nu = 0.001,
               control = rpart.control(maxdepth = 10, cp = 0.001),
               verbose = TRUE)

### save the model as a file
# saveRDS(ada_001, 'ada_001.rds')

### read the model from the file
# ada_001 <- read_rds('ada_001.rds')


### increase the memory limit in case of the problem of lacking memory
### e.g. Error: cannot allocate vector of size 136 Kb
memory.limit(size = 80000)

### make predictions on the test set and compute quality metrics
predicted_ada_001 <- predict(ada_001, newdata = flights_test)
confusionMatrix(predicted_ada_001, flights_test$DepDel15)

### compute AUC
cont_prediction_ada_001 <- as.vector(
  predict(ada_001, newdata = flights_test, type = 'prob')[, 2])
performance(prediction(cont_prediction_ada_001, flights_test$DepDel15),
            "auc")@y.values[[1]]  
### AUC is about 0.742886


### plot ROC
lines(performance(prediction(cont_prediction_ada_001, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_ada_001, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#ff0800')




### add a legend to the plot
legend('bottomright', 
       legend = c('tree_auc', 'tree_int', 'forest', 'ada_001'),
       col = c('#ffdf00', '#4169e1', '#228b22', '#ff0800'),
       lty = 1, lwd = 3,
       cex = 1.15)


### AdaBoost model shows the highest quality with AUC = 0.742886
### taking into account very low predictive power of most of the features
### it can be considered as a good quality of the model
