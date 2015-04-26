setwd("C:/dev/repositories/R/kaggle_otto")
source("init_ws.r")

i <- 9

engineered.data <- normalize(addFeatureCombination(i, munged.data))

set.seed(42)

split.data <- createDataPartition(engineered.data$is_class2, p = 0.6, list = FALSE)
t.data <- engineered.data[split.data, ]
tmp.test.data <- engineered.data[-split.data, ]

split.test <- createDataPartition(tmp.test.data$is_class2, p = 0.5, list = FALSE)

v.data <- tmp.test.data[split.test,]
ts.data <- tmp.test.data[-split.test,]

control.config <- trainControl(method = "repeatedcv", repeats = 1,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

#,181, 116, 171, 180, 122, 125, 177, 94, 124, 146, 154, 168
#126,103,
logreg.tmp.train <- train(is_class2 ~ .,
                          data = t.data %>% select(1:93,181, 116, 171, 180, 122, 125, 177, 94, 124, 146, 154, 168, 126,103, is_class2),
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred_train <- predict(logreg.tmp.train, t.data  %>% select(1:93,181, 116, 171, 180, 122, 125, 177, 94, 124, 146, 154, 168, 126,103, is_class2))
confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)$overall[1]

logreg.tmp.pred <- predict(logreg.tmp.train, v.data %>% select(1:93,181, 116, 171, 180, 122, 125, 177, 94, 124, 146, 154, 168, 126,103, is_class2))
confusionMatrix(logreg.tmp.pred, v.data$is_class2)$overall[1]


logreg.tmp.test <- predict(logreg.tmp.train, ts.data %>% select(1:93,181, 116, 171, 180, 122, 125, 177, 94, 124, 146, 154, 168, 126,103, is_class2))
confusionMatrix(logreg.tmp.test, ts.data$is_class2)$overall[1]