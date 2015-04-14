split.data <- createDataPartition(train.data$is_class1, p = 0.2, list = FALSE)
tmp.data <- train.data[split.data, ] %>% select(feat_1:feat_93, is_class2)
proc.data <- tmp.data %>% select(-is_class2)

names2 <- c(colnames(proc.data), paste(colnames(proc.data), "_2", sep=""))

proc.data <- cbind(proc.data, proc.data^2)
colnames(proc.data) <- names2
proc.data <- cbind(proc.data, is_class2 = tmp.data$is_class2)


logreg.train <- train(is_class2 ~ .,
                          data = proc.data,
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred <- predict(logreg.train, validation.data)
tmp.val.acc <- confusionMatrix(logreg.tmp.pred, validation.data$is_class2)$overall[1]

logreg.tmp.pred_train <- predict(logreg.train, proc.data)
tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, proc.data$is_class2)$overall[1]



svm.tune <- train(is_class2 ~ ., 
                  data = tmp.data,
                  method = "svmRadial",
                  tuneLength = 5,
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl = control.config)

logreg.tmp.pred <- predict(svm.tune, validation.data)
confusionMatrix(logreg.tmp.pred, validation.data$is_class2)

logreg.tmp.pred_train <- predict(svm.tune, proc.data)
confusionMatrix(logreg.tmp.pred_train, proc.data$is_class2)

svm.tune