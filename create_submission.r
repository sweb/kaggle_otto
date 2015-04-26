control.config <- trainControl(method = "repeatedcv", repeats = 1,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

logreg.train.class1.1 <- train(is_class1 ~ .,
                               data = munged.data %>% select(-(is_class2:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.train.class3.1 <- train(is_class3 ~ .,
                               data = munged.data %>% select(-(is_class1:is_class2), -(is_class4:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.train.class4.1 <- train(is_class4 ~ .,
                               data = munged.data %>% select(-(is_class1:is_class3), -(is_class5:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.train.class5.1 <- train(is_class5 ~ .,
                               data = munged.data %>% select(-(is_class1:is_class4), -(is_class6:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.train.class6.1 <- train(is_class6 ~ .,
                               data = munged.data %>% select(-(is_class1:is_class5), -(is_class7:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.train.class7.1 <- train(is_class7 ~ .,
                               data = munged.data %>% select(-(is_class1:is_class6), -(is_class8:is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.train.class8.1 <- train(is_class8 ~ .,
                               data = munged.data %>% select(-(is_class1:is_class7), -(is_class9)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.train.class9.1 <- train(is_class9 ~ .,
                               data = munged.data %>% select(-(is_class1:is_class8)),
                               method = "glm",
                               metric = "ROC",
                               trControl = control.config)

logreg.tmp.train <- train(is_class2 ~ .,
                          data = engineered.data,
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)


create_submission(logreg.train.class1.1, logreg.tmp.train, logreg.train.class3.1, logreg.train.class4.1, 
                  logreg.train.class5.1, logreg.train.class6.1, logreg.train.class7.1, logreg.train.class8.1,
                  logreg.train.class9.1)