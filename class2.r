require(dplyr)
require(ggplot2)
require(caret)

setwd("C:/dev/repositories/R/kaggle_otto")

source("otto_functions.r")

raw.data <- read.csv("data/train.csv", header = TRUE, sep = ",", na.strings = "")
munged.data <- munge_data(raw.data)


summary(munged.data)

factor.data <- raw.data %>% sapply(as.factor) %>% data.frame()

ggplot(data = factor.data) +
  theme_bw() +
  geom_bar(aes(x = feat_24))

non.zeros <- data.frame(factor.data %>% select(-id, -target) %>% sapply(function(c)sum(c!=0)))

non.zeros <- data.frame(feature = colnames(factor.data %>% select(-id, -target)), non.zeros)
colnames(non.zeros) <- c("feature", "nonzero")


non.zeros$feature <- reorder(non.zeros$feature, non.zeros$nonzero)
ggplot(data = data.frame(non.zeros %>% arrange(desc(nonzero)) %>% head(10))) +
  theme_bw() +
  geom_bar(aes(x = feature, y=nonzero), stat="identity") + 
  coord_flip()

summary(raw.data)

mod.data <- data.frame(raw.data, no_zeros = rowSums(raw.data==0))
rowSums(raw.data==0)

mod.data %>% group_by(target) %>% summarise(mean(no_zeros))

binary.data <- raw.data %>% select(-target, -id)
binary.data[binary.data > 0] <- 1

binary.data <- data.frame(binary.data, target = raw.data$target)

View(binary.data %>% group_by(target) %>% summarise_each(funs(mean)))

summary(mod.data)

raw.data %>% filter(feat_1 != 0) %>% group_by(target) %>% summarise(mean(feat_1))
raw.data %>% filter(feat_2 != 0) %>% group_by(target) %>% summarise(mean(feat_2))

correlation.1 <- cor(binary.data %>% filter(target == "Class_1") %>% select(-target))
correlation.2 <- cor(binary.data %>% filter(target == "Class_2") %>% select(-target))
correlation.6 <- cor(binary.data %>% filter(target == "Class_6") %>% select(-target))

correlation.1[correlation.1 > 0.4 & correlation.1  < 1]
correlation.2[correlation.2 < -0.2]
correlation.6[correlation.6 > 0.5 & correlation.6  < 1]
correlation.6 > 0.5 & correlation.6  < 1

qplot(x=Var1, y=Var2, data=melt(cor(binary.data %>% filter(target == "Class_6") %>% select(-target))), fill=value, geom="tile")


logreg.tmp.train <- train(is_class2 ~ .,
                          data = train.data %>% select(feat_1:feat_93, is_class2),
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred <- predict(logreg.tmp.train, validation.data)
tmp.val.acc <- confusionMatrix(logreg.tmp.pred, validation.data$is_class2)

logreg.tmp.prob <- predict(logreg.tmp.train, validation.data, type="prob")

logreg.tmp.pred_train <- predict(logreg.tmp.train, train.data)
tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, train.data$is_class2)



comp <- data.frame(result = logreg.tmp.pred, prob = logreg.tmp.prob, validation.data)
correct_class <- comp %>% filter(result == is_class2) %>% 
  filter(result == "Yes") %>% select(-(is_class1:is_class9))

wrong_class.yes <- comp %>% filter(result != is_class2) %>% 
  filter(result == "Yes") %>% select(-(is_class1:is_class9))

wrong_class.no <- comp %>% filter(result != is_class2) %>% 
  filter(result == "No") %>% select(-(is_class2))


calc.res <-logreg.tmp.train$finalModel$coefficients * cbind(1, validation.data[2,] %>% 
                                                   select(feat_1:feat_93))

ggplot(data = melt(calc.res) %>% filter(value != 0), aes(x=variable, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip()

sigmoid(rowSums(calc.res))

showCoefficients <- function(coeff, data, rowId) {
  res <-coeff * cbind(1, data[rowId,] %>% select(feat_1:feat_93))
  return (res)
}

tmp <- showCoefficients(logreg.tmp.train$finalModel$coefficients, correct_class,9)

ggplot(data = melt(tmp) %>% filter(value != 0), aes(x=variable, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip()

data.frame(row.names(logreg.tmp.train$finalModel$coefficients), melt(logreg.tmp.train$finalModel$coefficients)) %>% arrange(desc(value))