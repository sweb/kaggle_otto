setwd("C:/dev/repositories/R/kaggle_otto")

source("init_ws.r")

engineered.data <- munged.data


set.seed(42)
split.data <- createDataPartition(engineered.data$is_class2, p = 0.6, list = FALSE)
t.data <- engineered.data[split.data, ]
tmp.test.data <- engineered.data[-split.data, ]
set.seed(42)
split.test <- createDataPartition(tmp.test.data$is_class2, p = 0.5, list = FALSE)

v.data <- tmp.test.data[split.test,]
ts.data <- tmp.test.data[-split.test,]

control.config <- trainControl(method = "repeatedcv", repeats = 1,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

logreg.tmp.train <- train(is_class2 ~ .,
                          data = t.data %>% select(feat_1:feat_93, is_class2),
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred_train <- predict(logreg.tmp.train, t.data)
tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, t.data$is_class2, positive = 'Yes')

summary(logreg.tmp.pred_train)

data_vis <- data.frame(t.data %>% 
                         select(feat_1:feat_93, is_class2), 
             result = logreg.tmp.pred_train) %>%
  mutate(correct_yes = ifelse(is_class2 == result & is_class2 == "Yes", 1, 0),
         correct_no = ifelse(is_class2 == result & is_class2 == "No", 1, 0),
         wrong_yes = ifelse(is_class2 != result & is_class2 == "No", 1, 0),
         result_type = ifelse(correct_yes == 1, "Correct Yes",
                          ifelse(correct_no == 1, "Correct No",
                                 ifelse(wrong_yes == 1, "Wrong Yes","Wrong No"))),
         result_type = as.factor(result_type)
         ) %>%
  select(-correct_yes, -correct_no, -wrong_yes)

summary(data_vis)

ggplot(data_vis %>% filter(result_type == "Wrong Yes"), aes(x=feat_14, y=feat_48, color=result_type)) +
  theme_classic() +
  geom_point(alpha = 0.1)