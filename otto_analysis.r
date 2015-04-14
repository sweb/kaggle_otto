require(dplyr)
require(ggplot2)
require(Amelia)
require(caret)
require(pROC)

setwd("C:/dev/repositories/R/kaggle_otto")

raw.data <- read.csv("data/train.csv", header = TRUE, sep = ",", na.strings = "")

summary(raw.data)

missmap(raw.data, main="Otto classification - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

avg_feature_val <- raw.data %>%  select(feat_1:target) %>% group_by(target) %>% summarise_each(funs(mean))


munged.data <- raw.data %>%  select(-id) %>%
  mutate(is_class1 = ifelse(target == 'Class_1', 'Yes', 'No'), 
         is_class2 = ifelse(target == 'Class_2', 'Yes', 'No'),
         is_class3 = ifelse(target == 'Class_3', 'Yes', 'No'),
         is_class4 = ifelse(target == 'Class_4', 'Yes', 'No'),
         is_class5 = ifelse(target == 'Class_5', 'Yes', 'No'),
         is_class6 = ifelse(target == 'Class_6', 'Yes', 'No'),
         is_class7 = ifelse(target == 'Class_7', 'Yes', 'No'),
         is_class8 = ifelse(target == 'Class_8', 'Yes', 'No'),
         is_class9 = ifelse(target == 'Class_9', 'Yes', 'No')) %>%
  select(-target)

polished.data <- munged.data %>% mutate(is_class1 = as.factor(is_class1),
                                        is_class2 = as.factor(is_class2),
                                        is_class3 = as.factor(is_class3),
                                        is_class4 = as.factor(is_class4),
                                        is_class5 = as.factor(is_class5),
                                        is_class6 = as.factor(is_class6),
                                        is_class7 = as.factor(is_class7),
                                        is_class8 = as.factor(is_class8),
                                        is_class9 = as.factor(is_class9))

#polished.data$is_class1 <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))

correlation_matrix %>% filter(is_class1 > 0.1)

correlation_matrix <- data.frame(cor(munged.data)) %>%
  select(is_class1:is_class9)
correlation_matrix <- correlation_matrix[1:93,]

set.seed(42)

polished.data <- munged.data

split.data <- createDataPartition(munged.data$is_class1, p = 0.6, list = FALSE)
train.data <- munged.data[split.data, ]
tmp.test.data <- munged.data[-split.data, ]

split.test <- createDataPartition(tmp.test.data$is_class1, p = 0.5, list = FALSE)

validation.data <- tmp.test.data[split.test,]
test.data <- tmp.test.data[-split.test,]



cv.ctrl.class1 <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


logreg.train.class1.1 <- train(is_class1 ~ .,
                               data = train.data %>% select(-(is_class2:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class1.1 <- predict(logreg.train.class1.1, validation.data)
conf.class1 <- confusionMatrix(logreg.pred.class1.1, validation.data$is_class1)


logreg.train.class2.1 <- train(is_class2 ~ .,
                               data = train.data %>% select(-is_class1, -(is_class3:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class2.1 <- predict(logreg.train.class2.1, validation.data)
conf.class2 <- confusionMatrix(logreg.pred.class2.1, validation.data$is_class2)


logreg.train.class3.1 <- train(is_class3 ~ .,
                               data = train.data %>% select(-(is_class1:is_class2), -(is_class4:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class3.1 <- predict(logreg.train.class3.1, validation.data)
conf.class3 <- confusionMatrix(logreg.pred.class3.1, validation.data$is_class3)

#Class 4
logreg.train.class4.1 <- train(is_class4 ~ .,
                               data = train.data %>% select(-(is_class1:is_class3), -(is_class5:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class4.1 <- predict(logreg.train.class4.1, validation.data)
conf.class4 <- confusionMatrix(logreg.pred.class4.1, validation.data$is_class4)

#Class 5
logreg.train.class5.1 <- train(is_class5 ~ .,
                               data = train.data %>% select(-(is_class1:is_class4), -(is_class6:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class5.1 <- predict(logreg.train.class5.1, validation.data)
conf.class5 <- confusionMatrix(logreg.pred.class5.1, validation.data$is_class5)

rm(glm.tune.1, glm.tune.2, glm.tune.3, glm.tune.4)

#Class 6
logreg.train.class6.1 <- train(is_class6 ~ .,
                               data = train.data %>% select(-(is_class1:is_class5), -(is_class7:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class6.1 <- predict(logreg.train.class6.1, validation.data)
conf.class6 <- confusionMatrix(logreg.pred.class6.1, validation.data$is_class6)


#Class 7
logreg.train.class7.1 <- train(is_class7 ~ .,
                               data = train.data %>% select(-(is_class1:is_class6), -(is_class8:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class7.1 <- predict(logreg.train.class7.1, validation.data)
conf.class7 <- confusionMatrix(logreg.pred.class7.1, validation.data$is_class7)

#Class 8
logreg.train.class8.1 <- train(is_class8 ~ .,
                               data = train.data %>% select(-(is_class1:is_class7), -(is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class8.1 <- predict(logreg.train.class8.1, validation.data)
conf.class8 <- confusionMatrix(logreg.pred.class8.1, validation.data$is_class8)

#Class 9
logreg.train.class9.1 <- train(is_class9 ~ .,
                               data = train.data %>% select(-(is_class1:is_class8)),
                               method = "glm",
                               metric = "ROC",
                               trControl = cv.ctrl.class1)

logreg.pred.class9.1 <- predict(logreg.train.class9.1, validation.data)
conf.class9 <- confusionMatrix(logreg.pred.class9.1, validation.data$is_class9)


logreg.prob.class1.1 <- predict(logreg.train.class1.1, test.data, type="prob")
logreg.prob.class2.1 <- predict(logreg.train.class2.1, test.data, type="prob")
logreg.prob.class3.1 <- predict(logreg.train.class3.1, test.data, type="prob")
logreg.prob.class4.1 <- predict(logreg.train.class4.1, test.data, type="prob")
logreg.prob.class5.1 <- predict(logreg.train.class5.1, test.data, type="prob")
logreg.prob.class6.1 <- predict(logreg.train.class6.1, test.data, type="prob")
logreg.prob.class7.1 <- predict(logreg.train.class7.1, test.data, type="prob")
logreg.prob.class8.1 <- predict(logreg.train.class8.1, test.data, type="prob")
logreg.prob.class9.1 <- predict(logreg.train.class9.1, test.data, type="prob")

logreg.probs <- cbind(logreg.prob.class1.1$Yes,
                      logreg.prob.class2.1$Yes,
                      logreg.prob.class3.1$Yes,
                      logreg.prob.class4.1$Yes,
                      logreg.prob.class5.1$Yes,
                      logreg.prob.class6.1$Yes,
                      logreg.prob.class7.1$Yes,
                      logreg.prob.class8.1$Yes,
                      logreg.prob.class9.1$Yes)


colnames(logreg.probs) <- c("class_1", "class_2", "class_3", "class_4", "class_5",
                            "class_6", "class_7", "class_8", "class_9")


accus <- c(conf.class1$overall[1],
  conf.class2$overall[1],
  conf.class3$overall[1],
  conf.class4$overall[1],
  conf.class5$overall[1],
  conf.class6$overall[1],
  conf.class7$overall[1],
  conf.class8$overall[1],
  conf.class9$overall[1])

accus <- data.frame(accus) %>% mutate(accus = as.numeric(accus))
accus <- cbind(Class = paste("Class", 1:9, sep=""), accus)
ggplot(data = accus, aes(y=accus)) +
  theme_bw() +
  geom_bar(stat="identity", aes(x = Class, y=accus)) +
  coord_cartesian(ylim = c(0.8, 1))

write.csv(accus, file = "observations/accu_logreg.csv", quote = TRUE, row.names = TRUE)

glm.pred <- predict(glm.tune.4, test.data, type="prob")
df <- data.frame(logreg.probs)
df[, "max"] <- apply(df[,1:9],1,max)

class.pred <- df %>% mutate(class_1 = ifelse(class_1 == max, 1, 0),
              class_2 = ifelse(class_2 == max, 1, 0),
              class_3 = ifelse(class_3 == max, 1, 0),
              class_4 = ifelse(class_4 == max, 1, 0),
              class_5 = ifelse(class_5 == max, 1, 0),
              class_6 = ifelse(class_6 == max, 1, 0),
              class_7 = ifelse(class_7 == max, 1, 0),
              class_8 = ifelse(class_8 == max, 1, 0),
              class_9 = ifelse(class_9 == max, 1, 0)) %>% select(-max)


class.select <- class.pred %>% mutate(class = ifelse(class_1 == 1, 1, 
                                     ifelse(class_2 == 1, 2,
                                            ifelse(class_3 == 1, 3,
                                                   ifelse(class_4 == 1, 4,
                                                          ifelse(class_5 == 1, 5,
                                                                 ifelse(class_6 == 1, 6,
                                                                        ifelse(class_7 == 1, 7,
                                                                               ifelse(class_8 == 1, 8,
                                                                                      ifelse(class_9 == 1, 9, 0)))))))))) %>% select(class)


test.class.select <- test.data %>% mutate(class = ifelse(is_class1 == "Yes", 1, 
                                                      ifelse(is_class2 == "Yes", 2,
                                                             ifelse(is_class3 == "Yes", 3,
                                                                    ifelse(is_class4 == "Yes", 4,
                                                                           ifelse(is_class5 == "Yes", 5,
                                                                                  ifelse(is_class6 == "Yes", 6,
                                                                                         ifelse(is_class7 == "Yes", 7,
                                                                                                ifelse(is_class8 == "Yes", 8,
                                                                                                       ifelse(is_class9 == "Yes", 9, 0)))))))))) %>% select(class)



#before <- confusionMatrix(class.select$class, test.class.select$class)

after <- confusionMatrix(class.select$class, test.class.select$class)

summary(class.pred)

predict_data <- function(data) {
  
  prob.class1 <- predict(logreg.train.class1.1, data, type="prob")
  prob.class2 <- predict(logreg.train.class2.1, data, type="prob")
  prob.class3 <- predict(logreg.train.class3.1, data, type="prob")
  prob.class4 <- predict(logreg.train.class4.1, data, type="prob")
  prob.class5 <- predict(logreg.train.class5.1, data, type="prob")
  prob.class6 <- predict(logreg.train.class6.1, data, type="prob")
  prob.class7 <- predict(logreg.train.class7.1, data, type="prob")
  prob.class8 <- predict(logreg.train.class8.1, data, type="prob")
  prob.class9 <- predict(logreg.train.class9.1, data, type="prob")
  
  probs <- cbind(prob.class1$Yes,
                        prob.class2$Yes,
                        prob.class3$Yes,
                        prob.class4$Yes,
                        prob.class5$Yes,
                        prob.class6$Yes,
                        prob.class7$Yes,
                        prob.class8$Yes,
                        prob.class9$Yes) %>% round(6) %>% format(nsmall = 6)
  
  colnames(probs) <- c("Class_1", "Class_2", "Class_3", "Class_4", "Class_5",
                              "Class_6", "Class_7", "Class_8", "Class_9")
  
  tmp.probs <- data.frame(probs)
  tmp.probs[, "max"] <- apply(tmp.probs[,1:9],1,max)
  
  pred <- tmp.probs %>% mutate(Class_1 = ifelse(Class_1 == max, 1, 0),
                              Class_2 = ifelse(Class_2 == max, 1, 0),
                              Class_3 = ifelse(Class_3 == max, 1, 0),
                              Class_4 = ifelse(Class_4 == max, 1, 0),
                              Class_5 = ifelse(Class_5 == max, 1, 0),
                              Class_6 = ifelse(Class_6 == max, 1, 0),
                              Class_7 = ifelse(Class_7 == max, 1, 0),
                              Class_8 = ifelse(Class_8 == max, 1, 0),
                              Class_9 = ifelse(Class_9 == max, 1, 0)) %>% select(-max)
  
  res <- cbind( id = data %>% id, probs)
  return (res)
}


create_submission <- function() {
  data <- read.csv("data/test.csv", header = TRUE, sep = ",", na.strings = "")
  predictions <- predict_data(data)
  print(summary(predictions))
  write.csv(predictions, file = "result/submission.csv", quote = FALSE, row.names = FALSE)
}

test <- predict_data(raw.data)

write.csv(test %>% select(id:Class_9), file = "result/submission.csv", quote = FALSE, row.names = FALSE)


