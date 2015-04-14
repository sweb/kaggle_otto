require(dplyr)
require(ggplot2)
require(Amelia)
require(caret)

setwd("C:/dev/repositories/R/kaggle_otto")

raw.data <- read.csv("data/train.csv", header = TRUE, sep = ",", na.strings = "")
munged.data <- munge_data(raw.data)


set.seed(42)

split.data <- createDataPartition(munged.data$is_class1, p = 0.6, list = FALSE)
train.data <- munged.data[split.data, ]
tmp.test.data <- munged.data[-split.data, ]

split.test <- createDataPartition(tmp.test.data$is_class1, p = 0.5, list = FALSE)

validation.data <- tmp.test.data[split.test,]
test.data <- tmp.test.data[-split.test,]

control.config <- trainControl(method = "repeatedcv", repeats = 3,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

logreg.train.class1.1 <- train(is_class1 ~ .,
                               data = train.data %>% select(-(is_class2:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.pred.class1.1 <- predict(logreg.train.class1.1, validation.data)
confusionMatrix(logreg.pred.class1.1, validation.data$is_class1)


logreg.train.class1.2 <- train(is_class1 ~ .,
                               data = train.data %>% select(-(is_class2:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class1.2 <- predict(logreg.train.class1.2, validation.data)
confusionMatrix(logreg.pred.class1.1, validation.data$is_class1)


logreg.train.class2.1 <- train(is_class2 ~ .,
                               data = train.data %>% select(-is_class1, -(is_class3:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.1 <- predict(logreg.train.class2.1, validation.data)
confusionMatrix(logreg.pred.class2.1, validation.data$is_class2)

logreg.train.class2.2 <- train(is_class2 ~ feat_8 + feat_9 + feat_11 + feat_12 + feat_13 + feat_14 + feat_15 + feat_16 + feat_17 + feat_23 + feat_24 + feat_25 + feat_26 + feat_27 + feat_29 + feat_30 + feat_32 + feat_33 + feat_34 + feat_36 + feat_39 + feat_40 + feat_41 + feat_42 + feat_47 + feat_48 + feat_50 + feat_53 + feat_54 + feat_56 + feat_57 + feat_59 + feat_60 + feat_63 + feat_64 + feat_66 + feat_67 + feat_68 + feat_69 + feat_71 + feat_72 + feat_75 + feat_76 + feat_77 + feat_78 + feat_79 + feat_80 + feat_84 + feat_88 + feat_90 + feat_91,
                               data = train.data %>% select(-is_class1, -(is_class3:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.2 <- predict(logreg.train.class2.2, validation.data)
confusionMatrix(logreg.pred.class2.2, validation.data$is_class2)

logreg.train.class2.3 <- train(is_class2 ~ feat_11s + feat_26s + feat_33s + feat_34s + feat_42s + feat_48s + feat_11s3 + feat_26s3 + feat_33s3 + feat_34s3 + feat_42s3 + feat_48s3 + feat_8 + feat_9 + feat_11 + feat_12 + feat_13 + feat_14 + feat_15 + feat_16 + feat_17 + feat_23 + feat_25 + feat_26 + feat_27 + feat_29 + feat_30 + feat_32 + feat_33 + feat_34 + feat_36 + feat_39 + feat_40 + feat_41 + feat_42 + feat_47 + feat_48 + feat_50 + feat_53 + feat_56 + feat_57 + feat_59 + feat_60 + feat_63 + feat_64 + feat_66 + feat_67 + feat_68 + feat_69 + feat_71 + feat_72 + feat_75 + feat_76 + feat_77 + feat_78 + feat_79 + feat_80 + feat_84 + feat_88 + feat_90 + feat_91,
                               data = train.data %>% 
                                 select(-is_class1, -(is_class3:is_class9)) %>%
                                 mutate(feat_11s = feat_11*feat_11, 
                                        feat_26s = feat_26*feat_26, 
                                        feat_33s = feat_33*feat_33, 
                                        feat_34s = feat_34*feat_34, 
                                        feat_42s = feat_42*feat_42, 
                                        feat_48s = feat_48*feat_48,
                                        feat_11s3 = feat_11*feat_11*feat_11, 
                                        feat_26s3 = feat_26*feat_26*feat_26, 
                                        feat_33s3 = feat_33*feat_33*feat_33, 
                                        feat_34s3 = feat_34*feat_34*feat_34, 
                                        feat_42s3 = feat_42*feat_42*feat_42, 
                                        feat_48s3 = feat_48*feat_48*feat_48),
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.3 <- predict(logreg.train.class2.3, validation.data %>%
                                  mutate(feat_11s = feat_11*feat_11, 
                                         feat_26s = feat_26*feat_26, 
                                         feat_33s = feat_33*feat_33, 
                                         feat_34s = feat_34*feat_34, 
                                         feat_42s = feat_42*feat_42, 
                                         feat_48s = feat_48*feat_48,
                                         feat_11s3 = feat_11*feat_11*feat_11, 
                                         feat_26s3 = feat_26*feat_26*feat_26, 
                                         feat_33s3 = feat_33*feat_33*feat_33, 
                                         feat_34s3 = feat_34*feat_34*feat_34, 
                                         feat_42s3 = feat_42*feat_42*feat_42, 
                                         feat_48s3 = feat_48*feat_48*feat_48))

confusionMatrix(logreg.pred.class2.3, validation.data$is_class2)

train.data.class.2 <- train.data %>% 
  select(feat_14, feat_15, feat_48)

tmp.train <- cbind(train.data.class.2^2, train.data.class.2)
colnames(tmp.train) <- NULL
colnames(tmp.train) <- colnames(tmp.train, do.NULL = FALSE)
tmp.train.2 <- model.matrix(~.^2, tmp.train)

train.data.class.2 <- data.frame(tmp.train.2)
rm(tmp.train, tmp.train.2)
colnames(train.data.class.2) <- NULL
colnames(train.data.class.2) <- colnames(train.data.class.2, do.NULL = FALSE)
train.data.class.2 <- cbind(train.data.class.2, is_class2 = train.data$is_class2)

validation.data.class.2 <- validation.data %>% 
  select(feat_14, feat_15, feat_48)

tmp.train <- cbind(validation.data.class.2^2, validation.data.class.2)
colnames(tmp.train) <- NULL
colnames(tmp.train) <- colnames(tmp.train, do.NULL = FALSE)
tmp.train.2 <- model.matrix(~.^2, tmp.train)

validation.data.class.2 <- cbind(tmp.train.2)
rm(tmp.train, tmp.train.2)

colnames(validation.data.class.2) <- NULL
colnames(validation.data.class.2) <- colnames(validation.data.class.2, do.NULL = FALSE)
validation.data.class.2 <- cbind(validation.data.class.2, is_class2 = validation.data$is_class2)

logreg.train.class2.4 <- train(is_class2 ~ .,
                               data = train.data.class.2,
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.4 <- predict(logreg.train.class2.4, validation.data.class.2)
confusionMatrix(logreg.pred.class2.4, validation.data$is_class2)

logreg.pred_train.class2.4 <- predict(logreg.train.class2.4, train.data.class.2)
confusionMatrix(logreg.pred_train.class2.4, train.data$is_class2)

summary(logreg.train.class2.3)

class2.data <- munged.data %>% filter(is_class2 == "Yes")

feat_data <- munged.data %>% select(feat_1:feat_93)

feat_data * feat_data


class2.comparison <- munged.data %>% group_by(is_class2) %>% summarise_each(funs(mean))

logreg.train.class2.5 <- train(is_class2 ~ feat_2 + feat_3 + feat_11 + feat_14 + feat_15 + feat_19 + feat_24 + feat_25 + feat_26 + feat_27 + feat_40 + feat_46 + feat_54 + feat_58 + feat_60 + feat_69 + feat_90 + feat_91,
                               data = train.data %>% select(-is_class1, -(is_class3:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.5 <- predict(logreg.train.class2.5, validation.data)
confusionMatrix(logreg.pred.class2.5, validation.data$is_class2)
#+ feat_3 + feat_11 + feat_14 + feat_15 + feat_19 + feat_24 + feat_25 + feat_26 + feat_27 + feat_40 + feat_46 + feat_54 + feat_58 + feat_60 + feat_69 + feat_90 + feat_91


logreg.train.class2.6 <- train(is_class2 ~ feat_14 + feat_15 + feat_48,
                               data = train.data %>% select(-is_class1, -(is_class3:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.6 <- predict(logreg.train.class2.6, validation.data)
confusionMatrix(logreg.pred.class2.6, validation.data$is_class2)



train.data.class.2 <- train.data %>% 
  select(feat_33, feat_14, feat_15, feat_48, feat_25, feat_40)
#train.data.class.2^8, train.data.class.2^7, train.data.class.2^6, train.data.class.2^5, train.data.class.2^4, train.data.class.2^3, 
tmp.train <- cbind(train.data.class.2^2, train.data.class.2)
colnames(tmp.train) <- NULL
colnames(tmp.train) <- colnames(tmp.train, do.NULL = FALSE)
train.data.class.2 <- data.frame(cbind(tmp.train, is_class2 = train.data$is_class2))
rm(tmp.train)
###########################################################################
validation.data.class.2 <- validation.data %>% 
  select(feat_33, feat_14, feat_15, feat_48, feat_25, feat_40)
#validation.data.class.2^8, validation.data.class.2^7, validation.data.class.2^6, validation.data.class.2^5, validation.data.class.2^4, validation.data.class.2^3, 
tmp.train <- cbind(validation.data.class.2^2, validation.data.class.2)
colnames(tmp.train) <- NULL
colnames(tmp.train) <- colnames(tmp.train, do.NULL = FALSE)
validation.data.class.2 <- data.frame(tmp.train)
rm(tmp.train)
###########################################################################

logreg.train.class2.7 <- train(is_class2 ~ .,
                               data = train.data.class.2,
                               method = "glm",
                               metric = "ROC",
                               #preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.7 <- predict(logreg.train.class2.7, validation.data.class.2)
confusionMatrix(logreg.pred.class2.7, validation.data$is_class2)$overall[1]

logreg.pred_train.class2.7 <- predict(logreg.train.class2.7, train.data.class.2)
confusionMatrix(logreg.pred_train.class2.7, train.data$is_class2)$overall[1]


class2.8.train.data <- train.data %>% 
  select( feat_14, feat_15, feat_21, feat_19, feat_48, feat_69, feat_90, is_class2) %>%
  mutate(comb_feat1 = feat_14*feat_48, comb_feat2 = feat_15*feat_21, 
         comb_feat7 = feat_19 * feat_69)

class2.8.valid.data <- validation.data %>% 
  select( feat_14, feat_15, feat_21, feat_19, feat_48, feat_69, feat_90, is_class2) %>%
  mutate(comb_feat1 = feat_14*feat_48, comb_feat2 = feat_15*feat_21, 
         comb_feat7 = feat_19 * feat_69)

logreg.train.class2.8 <- train(is_class2 ~ .,
                               data = class2.8.train.data,
                               method = "glm",
                               metric = "ROC",
                               preProcess = c("center", "scale"),
                               trControl = control.config)

logreg.pred.class2.8 <- predict(logreg.train.class2.8, class2.8.valid.data)
confusionMatrix(logreg.pred.class2.8, validation.data$is_class2)

logreg.pred_train.class2.8 <- predict(logreg.train.class2.8, class2.8.train.data)
confusionMatrix(logreg.pred_train.class2.8, train.data$is_class2)$overall[1]
