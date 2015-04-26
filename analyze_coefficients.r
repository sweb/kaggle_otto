setwd("C:/dev/repositories/R/kaggle_otto")

source("init_ws.r")

require(reshape2)

engineered.data <- normalize(munged.data)

#set.seed(42)

split.data <- createDataPartition(engineered.data$is_class2, p = 0.6, list = FALSE)
t.data <- engineered.data[split.data, ]
tmp.test.data <- engineered.data[-split.data, ]

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


calc.res <-logreg.tmp.train$finalModel$coefficients * cbind(1, v.data[2,] %>% 
                                                              select(feat_1:feat_93))

ggplot(data = melt(calc.res) %>% filter(value != 0), aes(x=variable, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip()

sigmoid(rowSums(calc.res))

showCoefficients <- function(coeff, data, rowId) {
  res <-coeff * cbind(1, data[rowId,] %>% select(feat_1:feat_93))
  return (res)
}

multiplyCoefficients <- function(coeff, data) {
  res <-t(t(cbind(1, data %>% select(feat_1:feat_93))) * coeff)
  return (res)
}

tmp <- showCoefficients(logreg.tmp.train$finalModel$coefficients, wrong_class.no, 1)

ggplot(data = melt(tmp) %>% filter(value != 0), aes(x=variable, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip()

data.frame(row.names(logreg.tmp.train$finalModel$coefficients), melt(logreg.tmp.train$finalModel$coefficients)) %>% arrange(desc(value))

factored.correct_class <- correct_class %>% sapply(as.factor) %>% data.frame()
factored.wrong_class.no <- wrong_class.no %>% sapply(as.factor) %>% data.frame()

avg_feat_correct_class <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, correct_class) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_correct_class.no <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, correct_class.no) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_wrong_class.no <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, wrong_class.no) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_validation <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, v.data) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

joined_data <- inner_join(avg_feat_correct_class, avg_feat_wrong_class.no, by="variable") %>%
  inner_join(avg_feat_validation, by="variable") %>%
  rename(Correct = value.x, Wrong = value.y, Validation = value) %>%
  inner_join(avg_feat_correct_class.no, by="variable") %>%
  rename(Correct_No = value)

difference <- joined_data %>% 
  mutate(diff = abs(Correct - Wrong)) %>% top_n(15, diff) %>% arrange(desc(diff))


gather_diff <- difference %>% select(-diff) %>% gather("class", "value", 2:5)

write.csv(gather_diff, file = "observations/class2_diff_features.csv", quote = TRUE, row.names = TRUE)

ggplot(data = gather_diff, aes(x=reorder(variable, value))) +
  geom_bar(stat="identity", position="dodge", aes(y=value,fill=class)) +
  theme_minimal() +
  scale_fill_brewer(type="div", palette = "RdBu") +
  coord_flip()