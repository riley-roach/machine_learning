library(tidyverse)

wisc_bc_data <- read_csv('data/wisc_bc_data.csv')

library(caret)
library(caTools)
library(rpart.plot)
library(randomForest)
library(tree)
library(reprtree)
library(doMC)
library(broom)
library(sweep)

wisc_bc_data <-
  wisc_bc_data %>%
  # factoring diagnosis
  mutate(diagnosis = factor(
    diagnosis,
    levels = c('M',
               'B'),
    labels = c('Malignant',
               'Benign')
  ),
  # removing ID
  id = NULL)

wisc_bc_data[2:31] <-
  lapply(wisc_bc_data[2:31],
         scale)

set.seed(1)
train_set <-
  createDataPartition(y = wisc_bc_data$diagnosis,
                      p = 0.75,
                      list = FALSE)

model_train <- wisc_bc_data[train_set, ]
model_test <- wisc_bc_data[-train_set, ]

props <- function(x, y) {
  trn = prop.table(table(x))
  tst = prop.table(table(y))
  z = tibble(trn = round(trn,
                         digits = 4),
             tst = round(tst,
                         digits = 4))
  return(z)
}

props(model_train$diagnosis,
      model_test$diagnosis)

control <-
  trainControl(method = 'repeatedcv',
               repeats = 3,
               number = 10,
               search = 'random',
               verboseIter = TRUE,
               allowParallel = TRUE)

cl <- detectCores() - 1
registerDoMC(cl)
getDoParWorkers()

model <-
  train(diagnosis ~ .,
        data = model_train,
        method = 'rf',
        metric = 'Accuracy',
        tuneLength = 30,
        trControl = control)
registerDoSEQ()
model
confusionMatrix(model)
final_model <-
  tidy(model$finalModel$err.rate) %>% 
  gather(Class,
         Error,
         factor_key = FALSE)

final_model[final_model$Class == 'OOB', 1] <- 'Average'

final_model %>% 
  ggplot(aes(Class,
             Error)) +
  geom_violin(aes(color = Class,
                  fill = Class)) +
  labs(x = NULL,
       y = NULL,
       title = 'Classification Error Rates') +
  theme(legend.position = c(0.9,
                            0.9),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = '#B59397')) +
  scale_y_continuous(limits = c(min(final_model$Error),
                                0.14),
                     breaks = seq(0,
                                  0.15,
                                  0.03),
                     labels = seq(0,
                                  0.15,
                                  0.03)) +
  scale_color_manual(values = c('#8B65A4',
                                '#EF8D94',
                                '#677CA3')) +
  scale_fill_manual(values = c('#8B65A4',
                               '#EF8D94',
                               '#677CA3'))

model_pred <-
  predict(model,
          newdata = model_test)
confusionMatrix(model_pred,
                reference = model_test$diagnosis)
plot(model$finalModel)
legend(390,
       0.13,
       legend = c('Malignant',
                  'Average',
                  'Benign'),
       col = c('red',
               'black',
               'blue'),
       lty = c(1, 1, 1),
         lwd = c(2.5, 2.5, 2.5))


  
set.seed(1)
model_B <- randomForest(
  diagnosis ~ .,
  data = model_train,
  importance = TRUE,
  ntree = 500,
  mtry = 2,
  do.trace = 100
)
reprtree:::plot.getTree(model_B)
model_tree <- getTree(model_B)
plot(model_B)
