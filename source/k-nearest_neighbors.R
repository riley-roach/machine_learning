library(tidyverse)

wisc_bc_data <- read_csv('data/wisc_bc_data.csv')

library(caret)
library(caTools)

wisc_bc_data <-
  wisc_bc_data %>%
  # making diagnosis a factor
  mutate(diagnosis = factor(diagnosis,
                            levels = c('M',
                                       'B'),
                            labels = c('Malignant',
                                       'Benign')),
         # removing ID
         id = NULL)

wisc_bc_data[ ,2:31] <-
  lapply(wisc_bc_data[ ,2:31],
         as.numeric)

wisc_bc_data[2:31] <-
  # standardizing data
  lapply(wisc_bc_data[2:31],
         scale)

smallsum <- function(x) {
  # function to check for mean and standard deviations
  c(
    mean = mean(x),
    sd = sd(x))
}

lapply(wisc_bc_data[-1],
       smallsum)

set.seed(1)
train_set <-
  createDataPartition(y = wisc_bc_data$diagnosis,
                      p = 0.75,
                      list = FALSE)

train_wisc <- wisc_bc_data[train_set, ]
test_wisc <- wisc_bc_data[-train_set, ]
train_wisc$diagnosis %>%
  table() %>%
  prop.table()

test_wisc$diagnosis %>%
  table() %>%
  prop.table()

train_classes_wisc <-
  train_wisc$diagnosis
class(train_classes_wisc)

control <- trainControl(method = 'repeatedcv',
                        repeats = 6,
                        number = 10)

train_var_wisc <-
  train_wisc[ ,2:31]

set.seed(10)

model <-
  train(train_var_wisc,
        train_classes_wisc,
        method = 'knn',
        tuneLength = 15,
        metric = 'Accuracy',
        trControl = control)
model

confusionMatrix(model)
# model performances -- 
# sensitivity = TP/(TP + FN)
  rec <- 33.9/(33.9 + 3.3)
# specificity = TN/(TN + FP)
  spec <- 61.9/(61.9 + 0.9)
# precision = TP/(TP + FP)
  prec <- 33.9/(33.9 + 0.9)
cbind(rec, spec, prec)


set.seed(100)
model_pr <-
  predict(model,
          newdata = test_wisc)

confusionMatrix(model_pr,
                # model_pr performances -- 
                # sensitivity = TP/(TP + FN) :: 52/(52 + 1) ; recall
                # specificity = TN/(TN + FP) :: 89/(89 + 0)
                # precision = TP/(TP + FP) :: 52/(52 + 0)
                reference = test_wisc$diagnosis)

recall(model_pr,
       reference = test_wisc$diagnosis)

model_prob_pred <-
  predict(model,
          newdata = test_wisc,
          type = 'prob')
model_prob_pred

colAUC(model_prob_pred,
       test_wisc$diagnosis,
       plotROC = TRUE)
