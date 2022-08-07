### load libraries and data
### decode y labels (to make functions work properly)

library(tidyverse)
library(randomForest)
library(caret)
library(MLmetrics)
library(ROCR)


setwd('C:\\Users\\olial\\OneDrive\\Desktop\\Desktop\\Flights\\data')

flights <- read_csv('data_final.csv', na = "")
flights$DepDel15 <- ifelse(flights$DepDel15 == 1, 'Delayed', 'Not_Delayed')
flights$DepDel15 <- as.factor(flights$DepDel15)


set.seed(0)


### split the data into train and test sets
inTraining <- createDataPartition(flights$DepDel15, p = .8, list = FALSE)
flights_train <- flights[inTraining, ]
flights_test <- flights[-inTraining, ]



#### FOREST ####
### baseline model
# 3-4 minutes
forest <- randomForest(DepDel15~., flights_train)


### plot variable importances
### origin_relh, hour_grouped and origin_grouped are the most important features
varImpPlot(forest)


### get confusion matrix, ROC plot and AUC
predicted_forest <- predict(forest, newdata = flights_test, type = 'class')
confusionMatrix(predicted_forest, flights_test$DepDel15, positive = 'Delayed')
cont_prediction_rf <- predict(forest, newdata = flights_test, type = 'prob')
cont_prediction_rf <- as.vector(cont_prediction_rf[, 2])
plot(performance(prediction(cont_prediction_rf, flights_test$DepDel15),
                 'tpr', 'fpr'),
     lwd = 3, col = '#ff9f00')

perf_auc_rf <- performance(prediction(cont_prediction_rf, flights_test$DepDel15),"auc")
perf_auc_rf@y.values[[1]]  ### 0.7112


### forest error vs number of trees
plot(forest)  # errors of models with ntree above 250 are close



#### TUNE MTRY ####
### using ntree=250


### split training set into training and validation sets
inTraining2 <- createDataPartition(flights_train$DepDel15, p = .8, list = FALSE)
flights_valid <- flights_train[-inTraining2, ]
flights_train2 <- flights_train[inTraining2, ]


### build a model for every possible mtry value

# list of models
forests <- list()

# build 15 forests --- about 20 minutes
for (i in 1:15){
  forests[[i]] <- randomForest(DepDel15~., flights_train2, 
                               mtry = i, ntree = 250)
  cat(i, '\n')
}


# lists of predicted probabilities and AUC for each model
cont_prediction_list <- list()
perf_auc_list <- vector()

# compute AUC for every forest --- 1-2 minutes
for (i in 1:15){
  cont_prediction_m <- as.vector(predict(forests[[i]], newdata = flights_valid, type = 'prob')[, 2])
  cont_prediction_list[[i]] <- cont_prediction_m
  
  perf_auc_m <- performance(prediction(cont_prediction_m, flights_valid$DepDel15),"auc")
  perf_auc_list <- c(perf_auc_list, perf_auc_m@y.values[[1]])
  
  cat(i, '\n')
}


# compute SE, SP, ACC for every forest --- 1-2 minutes
class_prediction_list <- list()
cm_list <- list()
acc_list <- vector()
sens_list <- vector()
spec_list <- vector()

for (i in 1:15){
  class_prediction_m <- as.factor(predict(forests[[i]], newdata = flights_valid, type = 'class'))
  class_prediction_list[[i]] <- class_prediction_m
  
  cm_m <- confusionMatrix(class_prediction_m, flights_valid$DepDel15)
  cm_list[[i]] <- cm_m
  
  acc_list <- c(acc_list, cm_m$overall[[1]])
  sens_list <- c(sens_list, cm_m$byClass[[1]])
  spec_list <- c(spec_list, cm_m$byClass[[2]])
  
  cat(i, '\n')
}


### plot AUC, ACC, SE, SP for every mtry value

library(gridExtra)

auc_plot <- ggplot(as.data.frame(perf_auc_list),
       aes(x = seq(1, 15, 1),
           y = perf_auc_list)) + 
  geom_path(color = '#4169e1', size = 1.25) +
  ylim(c(0.66, 0.716)) +
  scale_x_discrete(limits = seq(1, 15, 1)) +
  labs(x = 'mtry', y = NULL, title = 'AUC (Validation Set)') +
  theme_bw()

sens_plot <- ggplot(as.data.frame(sens_list),
       aes(x = seq(1, 15, 1),
           y = sens_list)) + 
  geom_path(color = '#4169e1', size = 1.25) +
  scale_x_discrete(limits = seq(1, 15, 1)) +
  labs(x = 'mtry', y = NULL, title = 'Sensitivity (Validation set)') +
  theme_bw()

spec_plot <- ggplot(as.data.frame(spec_list),
       aes(x = seq(1, 15, 1),
           y = spec_list)) + 
  geom_path(color = '#4169e1', size = 1.25) +
  scale_x_discrete(limits = seq(1, 15, 1)) +
  labs(x = 'mtry', y = NULL, title = 'Specificity (Validation set)') +
  theme_bw()

acc_plot <- ggplot(as.data.frame(acc_list),
       aes(x = seq(1, 15, 1),
           y = acc_list)) + 
  geom_path(color = '#4169e1', size = 1.25) +
  scale_x_discrete(limits = seq(1, 15, 1)) +
  labs(x = 'mtry', y = NULL, title = 'Accuracy (Validation set)') +
  theme_bw()

grid.arrange(auc_plot, acc_plot, sens_plot, spec_plot,
             ncol = 1)

### increasing mtry from 3 (default) to 10 increases SE from 0.084 to 0.113
### AUC and ACC of these models are close
### (AUC: 0.703 vs 0.702, ACC: 0.8203 vs 0.8201)



#### FOREST2 ####
forest2 <- randomForest(DepDel15~., flights_train, 
                        mtry = 10, ntree = 500)

# confusion matrix
predicted_forest2 <- predict(forest2, newdata = flights_test, type = 'class')
confusionMatrix(predicted_forest2, flights_test$DepDel15)

# predicted probabilities for ROC plot
cont_prediction_rf2 <- predict(forest2, newdata = flights_test, type = 'prob')
cont_prediction_rf2 <- as.vector(cont_prediction_rf2[, 2])

# AUC of forest2 
perf_auc_rf2 <- performance(prediction(cont_prediction_rf2, flights_test$DepDel15),"auc")
perf_auc_rf2@y.values[[1]]  ### 0.7112




### ROC of forest(mtry=3) and forest2(mtry=3)
### curves are close to each other --> quality of models is about the same

plot(performance(prediction(cont_prediction_rf2, flights_test$DepDel15),
                 'tpr', 'fpr'),
     lwd = 3, col = '#4169e1')
lines(performance(prediction(cont_prediction_rf, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_rf, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#ff9f00')
legend('bottomright', 
       legend = c('forest', 'forest2'),
       col = c('#ff9f00', '#4169e1'),
       lty = 1, lwd = 3,
       cex = 1.15)
