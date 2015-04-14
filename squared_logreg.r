split.data <- createDataPartition(train.data$is_class1, p = 0.2, list = FALSE)
tmp.data <- train.data[split.data, ] %>% select(feat_1:feat_93, is_class2)
proc.data <- tmp.data %>% select(-is_class2)

tmp.train <- model.matrix(~.^2, proc.data)
names2 <- c(paste(colnames(proc.data), "_2", sep=""))
colnames(tmp.train) <- names2


proc.data_s <- proc.data^2
names3 <- gsub(":","_",colnames(tmp.train))

square_model <- function(data) {
  t.data <- data %>% select(feat_1:feat_93, is_class2)
  p.data <- t.data %>% select(-is_class2)
  
  t.train <- model.matrix(~.^2, p.data)
  n3 <- gsub(":","_",colnames(t.train))
  colnames(t.train) <- n3
  
  p.data_s <- p.data^2
  n2 <- c(paste(colnames(p.data), "_2", sep=""))
  colnames(p.data_s) <- n2
  
  print(nrow(t.train))
  print(nrow(p.data_s))
  print(nrow(t.data))
  
  res <- data.frame(t.train, p.data_s, is_class2 = t.data$is_class2) %>% select(-1)
  
  return (res)
}


tmp.valid.squared <- square_model(validation.data)

tmp.train.squared <- square_model(train.data[split.data, ])

logreg.tmp.train <- train(is_class2 ~ .,
                          data = tmp.train.squared,
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred <- predict(logreg.tmp.train, tmp.valid.squared)
tmp.val.acc <- confusionMatrix(logreg.tmp.pred, validation.data$is_class2)

logreg.tmp.pred_train <- predict(logreg.tmp.train, tmp.data)
tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, tmp.data$is_class2)$overall[1]