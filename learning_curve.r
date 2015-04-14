require(dplyr)
require(ggplot2)
require(Amelia)
require(caret)

setwd("C:/dev/repositories/R/kaggle_otto")

source("otto_functions.r")

c <- data.frame()

for (i in seq(0.01,0.3,0.01)) {
  print("Processing...")
  split.data <- createDataPartition(train.data$is_class1, p = i, list = FALSE)
  tmp.data <- train.data[split.data, ]
  
  logreg.tmp.train <- train(is_class2 ~ .,
                                 data = tmp.data %>% select(feat_1:feat_93, is_class2),
                                 method = "glm",
                                 metric = "ROC",
                                 trControl = control.config)
  
  logreg.tmp.pred <- predict(logreg.tmp.train, validation.data)
  tmp.val.acc <- confusionMatrix(logreg.tmp.pred, validation.data$is_class2)$overall[1]
  
  logreg.tmp.pred_train <- predict(logreg.tmp.train, tmp.data)
  tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, tmp.data$is_class2)$overall[1]
  c <- rbind(c, cbind(percentage = i, train = tmp.train.acc, valid = tmp.val.acc))
}
write.csv(c, file = "observations/learning_curve.csv", quote = TRUE, row.names = TRUE)
ggplot(data = c) +
  theme_bw() +
  geom_point(aes(x=percentage, y=train, colour="Training")) +
  geom_point(aes(x=percentage, y=valid, colour="Validation")) +
  geom_smooth(aes(x=percentage, y=train), colour="red", method = "loess") +
  geom_smooth(aes(x=percentage, y=valid), method = "loess") +
  scale_colour_manual("",
                      breaks = c("Training", "Validation"),
                      values = c("red", "blue")) +
  ggtitle("Learning curve Class 2") +
  xlab("Percentage of training set") +
  ylab("Accuracy")
  