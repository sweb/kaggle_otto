setwd("C:/dev/repositories/R/kaggle_otto")

source("init_ws.r")

require(reshape2)
require (tidyr)

engineered.data <- normalize(munged.data)


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

logreg.tmp.pred <- predict(logreg.tmp.train, v.data)
tmp.val.acc <- confusionMatrix(logreg.tmp.pred, v.data$is_class2)

logreg.tmp.prob <- predict(logreg.tmp.train, v.data, type="prob")

logreg.tmp.pred_train <- predict(logreg.tmp.train, t.data)
tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)



comp <- data.frame(result = logreg.tmp.pred, prob = logreg.tmp.prob, v.data)
correct_class <- comp %>% filter(result == is_class2) %>% 
  filter(result == "Yes")

correct_class.no <- comp %>% filter(result == is_class2) %>% 
  filter(result == "No")

wrong_class.yes <- comp %>% filter(result != is_class2) %>% 
  filter(result == "Yes")

wrong_class.no <- comp %>% filter(result != is_class2) %>% 
  filter(result == "No")


multiplyCoefficients <- function(coeff, data) {
  res <-t(t(cbind(1, data %>% select(feat_1:feat_93))) * coeff)
  return (res)
}


avg_feat_correct_class <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, correct_class) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_correct_class.no <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, correct_class.no) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_wrong_class.no <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, wrong_class.no) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_validation <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, v.data) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_wrong_class.yes <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, wrong_class.yes) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

joined_data <- inner_join(avg_feat_correct_class, avg_feat_wrong_class.no, by="variable") %>%
  inner_join(avg_feat_validation, by="variable") %>%
  rename(Correct = value.x, Wrong = value.y, Validation = value) %>%
  inner_join(avg_feat_correct_class.no, by="variable") %>%
  rename(Correct_No = value)

difference <- joined_data %>% 
  mutate(diff = abs(Correct - Wrong)) %>% top_n(15, diff) %>% arrange(desc(diff))


gather_diff <- difference %>% select(-diff) %>% gather("class", "value", 2:5)

#write.csv(gather_diff, file = "observations/class2_diff_features.csv", quote = TRUE, row.names = TRUE)

ggplot(data = gather_diff, aes(x=reorder(variable, value))) +
  geom_bar(stat="identity", position="dodge", aes(y=value,fill=class)) +
  theme_minimal() +
  scale_fill_brewer(type="div", palette = "RdBu") +
  coord_flip()

both_higher <- joined_data %>% 
  filter(Correct < Validation & Wrong < Validation & Correct < 0) %>% 
  mutate(diff = abs(Correct-Validation)) %>%
  top_n(15, diff)


opposite_joined_data <- inner_join(avg_feat_correct_class.no, avg_feat_wrong_class.yes, by="variable") %>%
  inner_join(avg_feat_validation, by="variable") %>%
  rename(Correct = value.x, Wrong = value.y, Validation = value) %>%
  inner_join(avg_feat_correct_class, by="variable") %>%
  rename(Correct_Yes = value)

opposite_difference <- opposite_joined_data %>% 
  mutate(diff = abs(Correct - Wrong)) %>% top_n(15, diff) %>% arrange(desc(diff))


opposite_gather_diff <- opposite_difference %>% select(-diff) %>% gather("class", "value", 2:5)

ggplot(data = opposite_gather_diff, aes(x=reorder(variable, value))) +
  geom_bar(stat="identity", position="dodge", aes(y=value,fill=class)) +
  theme_minimal() +
  scale_fill_brewer(type="div", palette = "RdBu") +
  coord_flip()

opposite_joined_data %>% 
  filter(Correct > Validation & Wrong > Validation & Correct > 0) %>% 
  mutate(diff = abs(Correct-Validation)) %>%
  top_n(15, diff)