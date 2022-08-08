### load libraries and data
### decode y labels (to make functions work properly)

library(tidyverse)
library(ada)
library(plyr)
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



#### ADA_DEFAULT ####
### train a baseline model
# 3-4 minutes
ada_default <- ada(DepDel15~., data = flights_train,
                   verbose = TRUE)

### get confusion matrix, ROC plot and AUC
predicted_default <- predict(ada_default, newdata = flights_test)
confusionMatrix(predicted_default, flights_test$DepDel15)

cont_prediction_default <- predict(ada_default, newdata = flights_test, type = 'prob')
cont_prediction_default <- as.vector(cont_prediction_default[, 2])
plot(performance(prediction(cont_prediction_default, flights_test$DepDel15),
                 'tpr', 'fpr'),
     lwd = 3, col = '#4169e1')

perf_auc_default <- performance(prediction(cont_prediction_default, flights_test$DepDel15),"auc")
perf_auc_default@y.values[[1]] ### 0.7045



#### GRID SEARCH 1 ####
### tune learning rate and maxdepths

### create a validation dataset
inTraining2 <- createDataPartition(flights_train$DepDel15, p = .8, list = FALSE)
flights_valid <- flights_train[-inTraining2, ]
flights_train2 <- flights_train[inTraining2, ]


### df to store performance measures for different hyperparameters values
perf_df <- data.frame(iter = NULL, nu = NULL, maxdepth = NULL,
                      AUC = NULL, ACC = NULL, Sens = NULL, Spec = NULL)

### df to store performance for the current model
result <- data.frame(iter = NA, nu = NA, maxdepth = NA,
                     AUC = NA, ACC = NA, Sens = NA, Spec = NA)

### hyperparameters values for testing
iters <- c(300)
nus <- c(0.1, seq(0.2, 1, by = 0.2))
maxdepths <- c(1, 2, 3, 4, 5, 10, 30)
i = 0


for (iter_i in iters){
  for (nu_i in nus){
    for (maxdepth_i in maxdepths){
      result$iter <- iter_i
      result$nu <- nu_i
      result$maxdepth <- maxdepth_i
      
      model <- ada(DepDel15~., data = flights_train2,
                   iter = iter_i, nu = nu_i,
                   control = rpart.control(maxdepth = maxdepth_i))
      
      # AUC
      cont_prediction_i <- as.vector(predict(model, newdata = flights_valid, type = 'prob')[, 2])
      perf_auc_i <- performance(prediction(cont_prediction_i, flights_valid$DepDel15),"auc")
      result$AUC <-  perf_auc_i@y.values[[1]]
      
      # Confusion matrix
      class_prediction_i <- as.factor(predict(model, newdata = flights_valid, type = 'class'))
      cm_i <- confusionMatrix(class_prediction_i, flights_valid$DepDel15)
      
      # Measures
      result$ACC <-  cm_i$overall[[1]]
      result$Sens <- cm_i$byClass[[1]]
      result$Spec <- cm_i$byClass[[2]]
      
      perf_df <- bind_rows(perf_df, result)
      
      # Counter to track the progress
      i <- i + 1
      cat(i, '\n')
    }
  }
}
### save df with measures values
write_csv(perf_df, 'perf_df.csv')


### plot AUC vs learning rate and maxdepth 
ggplot(perf_df, aes(x = nu, y = maxdepth, fill = AUC)) +
  geom_tile()



#### GRID SEARCH 2 ####
### tuning complexity parameter, maxdepth, learning rate and number of trees
### taking into account the results of the first round of grid search

### df to store performance measures for different hyperparameters values
perf_df2 <- data.frame(iter = NULL, nu = NULL, maxdepth = NULL, cp = NULL,
                      AUC = NULL, ACC = NULL, Sens = NULL, Spec = NULL)

### df to store performance for the current model
result <- data.frame(iter = NA, nu = NA, maxdepth = NA, cp = NA,
                     AUC = NA, ACC = NA, Sens = NA, Spec = NA)

iters <- c(25, 100)
nus <- c(0.001, 0.01, 0.25, 0.5, 1)
maxdepths <- c(1, 2, 10, 30)
cps <- c(0.001, 0.01, 0.1)
i = 0


for (iter_i in iters){
  for (nu_i in nus){
    for (maxdepth_i in maxdepths){
      for (cp_i in cps){
        result$iter <- iter_i
        result$nu <- nu_i
        result$maxdepth <- maxdepth_i
        result$cp <- cp_i
        
        model <- ada(DepDel15~., data = flights_train2,
                     iter = iter_i, nu = nu_i,
                     control = rpart.control(maxdepth = maxdepth_i,
                                             cp = cp_i))
        
        # AUC
        cont_prediction_i <- as.vector(predict(model, newdata = flights_valid, type = 'prob')[, 2])
        perf_auc_i <- performance(prediction(cont_prediction_i, flights_valid$DepDel15),"auc")
        result$AUC <-  perf_auc_i@y.values[[1]]
        
        # Confusion matrix
        class_prediction_i <- as.factor(predict(model, newdata = flights_valid, type = 'class'))
        cm_i <- confusionMatrix(class_prediction_i, flights_valid$DepDel15)
        
        # Measures
        result$ACC <-  cm_i$overall[[1]]
        result$Sens <- cm_i$byClass[[1]]
        result$Spec <- cm_i$byClass[[2]]
        
        perf_df2 <- bind_rows(perf_df2, result)
        
        i <- i + 1
        cat(i, '\n')
      }
    }
  }
}
write_csv(perf_df2, 'perf_df2.csv')



#### FINAL MODELS ####
### based on the results of the grid search
### build and evaluate 2 models with different learning rate


ada_1 <- ada(DepDel15~., data = flights_train,
             iter = 2000, nu = 1,
             control = rpart.control(maxdepth = 10, cp = 0.001),
             verbose = TRUE)

predicted_ada_1 <- predict(ada_1, newdata = flights_test)
confusionMatrix(predicted_ada_1, flights_test$DepDel15)

cont_prediction_ada_1 <- as.vector(predict(ada_1, newdata = flights_test, type = 'prob')[, 2])
perf_auc_ada_1 <- performance(prediction(cont_prediction_ada_1, flights_test$DepDel15),"auc")
perf_auc_ada_1@y.values[[1]]
### AUC is about 0.6728


ada_001 <- ada(DepDel15~., data = flights_train,
              iter = 2000, nu = 0.01,
              control = rpart.control(maxdepth = 10, cp = 0.001),
              verbose = TRUE)

predicted_ada_001 <- predict(ada_001, newdata = flights_test)
confusionMatrix(predicted_ada_001, flights_test$DepDel15)

cont_prediction_ada_001 <- as.vector(predict(ada_001, newdata = flights_test, type = 'prob')[, 2])
perf_auc_ada_001 <- performance(prediction(cont_prediction_ada_001, flights_test$DepDel15),"auc")
perf_auc_ada_001@y.values[[1]] 
### AUC is about 0.7185


### plot ROCs of both models

plot(performance(prediction(cont_prediction_ada_1, flights_test$DepDel15),
                 'tpr', 'fpr'),
     lwd = 3, col = '#4169e1', alpha = 0.7)
lines(performance(prediction(cont_prediction_ada_001, flights_test$DepDel15),
                  'tpr', 'fpr')@x.values[[1]],
      performance(prediction(cont_prediction_ada_001, flights_test$DepDel15),
                  'tpr', 'fpr')@y.values[[1]],
      lwd = 3, col = '#ff9f00', alpha = 0.7)
legend('bottomright', 
       legend = c('ada_1', 'ada_001'),
       col = c('#4169e1', '#ff9f00'),
       lty = 1, lwd = 3,
       cex = 1.15)


### ROC of ada_001 is above ROC of ada_1
### AUC of ada_001 is higher, than AUC of ada_1
### ada_001 shows better quality on the test set
