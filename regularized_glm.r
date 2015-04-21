setwd("C:/dev/repositories/R/kaggle_otto")

mat <- data.matrix(train.data %>% select(feat_1:feat_93))

norm_mat <- apply(mat, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

normalize <- function(df) {
  mat <- data.matrix(df %>% select(feat_1:feat_93))
  norm_mat <- apply(mat, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
  data <- data.frame(norm_mat, df %>% select(is_class2))
  return (data)
}

data <- data.frame(norm_mat, train.data %>% select(is_class2))
valid_data <- normalize(validation.data)

logreg.tmp.train <- train(is_class2 ~ .,
                          data = data %>% select(feat_1:feat_93, is_class2),
                          method = "glmnet",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred <- predict(logreg.tmp.train, valid_data)
tmp.val.acc <- confusionMatrix(logreg.tmp.pred, valid_data$is_class2)$overall[1]

logreg.tmp.pred_train <- predict(logreg.tmp.train, data)
tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, data$is_class2)$overall[1]
c <- rbind(c, cbind(percentage = i, train = tmp.train.acc, valid = tmp.val.acc))

new_feat <- data$feat_26 * select(data, feat_1:feat_93, -feat_26)
colnames(new_feat) <- paste("feat_26",colnames(new_feat), sep="_")

new_data <- data.frame(data, new_feat)

logreg.tmp.train <- train(is_class2 ~ .,
                          data = new_data,
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred_train <- predict(logreg.tmp.train, new_data)
confusionMatrix(logreg.tmp.pred_train, new_data$is_class2)