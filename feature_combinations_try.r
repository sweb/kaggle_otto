setwd("C:/dev/repositories/R/kaggle_otto")
source("init_ws.r")

i <- 14

set.seed(42)
feat_9_combinations <- normalize(addFeatureCombination(9, munged.data)) %>% 
  select(1:93, 181, 116, 171, 180, 122, 177, 94, 154, 168, 126,103, is_class2)
feat_14_combinations <- normalize(addFeatureCombination(14, munged.data)) %>%
  select(169,126,172,122,181,121)
feat_25_combinations <- normalize(addFeatureCombination(25, munged.data)) %>%
  select(103,107,108,111,122,124,126,128,132,137,152,158,161,166,169,170,176,181)
#feat_89_combinations <- normalize(addFeatureCombination(89, munged.data))

feat_40_combinations <- normalize(addFeatureCombination(40, munged.data)) %>% 
  select(101,104,107,108,110,117,123,126,127,140,152,159,161,166,169,170,181,184,185)

feat_33_combinations <- normalize(addFeatureCombination(33, munged.data)) %>% 
  select(104,107,108,117,143,154,156,158,164,166,168,171)

feat_15_combinations <- normalize(addFeatureCombination(15, munged.data)) %>% 
  select(102, 103, 104, 108, 116, 117, 122, 123, 125, 126, 130, 134, 135, 140, 151, 154, 155, 159, 163, 164, 165, 171, 172, 176, 178, 181, 184)

engineered.data <- data.frame(feat_9_combinations, feat_14_combinations, feat_25_combinations, feat_40_combinations, feat_15_combinations)

#engineered.data <- normalize(addFeatureCombination(15, munged.data)) %>%
#  select(1:93, 102, 103, 104, 108, 116, 117, 122, 123, 125, 126, 130, 134, 135, 140, 151, 154, 155, 159, 163, 164, 165, 171, 172, 176, 178, 181, 184, is_class2)


#engineered.data <- normalize(munged.data)

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
                          data = t.data,
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred_train <- predict(logreg.tmp.train, t.data)
confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)$overall[1]

logreg.tmp.pred <- predict(logreg.tmp.train, v.data)
confusionMatrix(logreg.tmp.pred, v.data$is_class2)$overall[1]


logreg.tmp.test <- predict(logreg.tmp.train, ts.data)
confusionMatrix(logreg.tmp.test, ts.data$is_class2)$overall[1]