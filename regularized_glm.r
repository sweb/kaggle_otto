setwd("C:/dev/repositories/R/kaggle_otto")

mat <- data.matrix(train.data %>% select(feat_1:feat_93))

norm_mat <- apply(mat, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

normalize <- function(df) {
  mat <- data.matrix(df %>% select(-(is_class1:is_class9)))
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

a <- Sys.time()
logreg.tmp.train <- train(is_class2 ~ .,
                          data = train.data %>% select(feat_1:feat_93, is_class2),
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)
Sys.time() - a

logreg.tmp.pred_train <- predict(logreg.tmp.train, train.data %>% select(feat_1:feat_93, is_class2))
confusionMatrix(logreg.tmp.pred_train, train.data$is_class2)

logreg.tmp.pred <- predict(logreg.tmp.train, validation.data)
tmp.val.acc <- confusionMatrix(logreg.tmp.pred, validation.data$is_class2)$overall[1]


addFeatureCombination <- function(featureId, data) {
  new_feat <- data[,featureId] * select(data, feat_1:feat_93, -featureId)
  colnames(new_feat) <- paste(colnames(select(data, featureId)),colnames(new_feat), sep="_")
  
  new_data <- data.frame(data, new_feat)
  return (new_data)
}



c <- data.frame()

c <- read.csv(file = "observations/feature_combinations.csv") %>% data.frame() %>% select(-X)

for (i in seq(71,93)) {
  print (paste("Starting", i))
  engineered.data <- normalize(addFeatureCombination(i, munged.data))
  
  split.data <- createDataPartition(engineered.data$is_class2, p = 0.6, list = FALSE)
  t.data <- engineered.data[split.data, ]
  tmp.test.data <- engineered.data[-split.data, ]
  
  split.test <- createDataPartition(tmp.test.data$is_class2, p = 0.5, list = FALSE)
  
  v.data <- tmp.test.data[split.test,]
  ts.data <- tmp.test.data[-split.test,]
  
  a <- Sys.time()
  logreg.tmp.train <- train(is_class2 ~ .,
                            data = t.data,
                            method = "glm",
                            metric = "ROC",
                            trControl = control.config)
  print (Sys.time())
  print (Sys.time() - a)
  
  logreg.tmp.pred_train <- predict(logreg.tmp.train, t.data)
  tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)$overall[1]
  
  logreg.tmp.pred <- predict(logreg.tmp.train, v.data)
  tmp.val.acc <- confusionMatrix(logreg.tmp.pred, v.data$is_class2)$overall[1]
  print(paste("Train:", tmp.train.acc, "- Validation:", tmp.val.acc))
  c <- rbind(c, cbind(feat = i, train = tmp.train.acc, validation = tmp.val.acc))
}
print (paste("End at",Sys.time()))

write.csv(c, file = "observations/feature_combinations.csv", quote = TRUE)