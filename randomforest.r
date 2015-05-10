setwd("C:/dev/repositories/R/kaggle_otto")
source("init_ws.r")



require(randomForest)

data <- munged.data %>% select(1:93)


yes_class_data <- train.data %>% filter(is_class2 == "Yes")
set.seed(42)
no_class_data <- train.data %>% filter(is_class2 == "No") %>% sample_n(16122)

data_comp <- rbind(yes_class_data, no_class_data)
data <- data_comp %>% select(1:93, is_class2)

data <- addFeatureCombination(9, munged.data %>% select(1:93)) %>%
  select(-feat_9_feat_84,-feat_9_feat_51,-feat_9_feat_61,-feat_9_feat_6,-feat_9_feat_5,-feat_9_feat_82,-feat_9_feat_81,-feat_9_feat_91,-feat_9_feat_28,-feat_9_feat_45,-feat_84,-feat_9_feat_7,-feat_9_feat_31,-feat_9_feat_2,-feat_9_feat_57,-feat_51,-feat_9_feat_29,-feat_9_feat_30,-feat_9_feat_77,-feat_9_feat_47,-feat_9_feat_49,-feat_9_feat_93,-feat_9_feat_3,-feat_9_feat_79,-feat_9_feat_83,-feat_9_feat_26,-feat_9_feat_46,-feat_9_feat_19,-feat_9_feat_58,-feat_9_feat_23)

data <- addFeatureCombination(25, munged.data %>% select(1:93)) %>%
  select(-feat_25_feat_51,-feat_25_feat_84,-feat_84,-feat_51,-feat_25_feat_61,-feat_25_feat_82,-feat_25_feat_6,-feat_25_feat_81,-feat_25_feat_2,-feat_82,-feat_25_feat_93,-feat_81,-feat_25_feat_91,-feat_25_feat_28,-feat_25_feat_31,-feat_6,-feat_25_feat_7,-feat_25_feat_5,-feat_25_feat_47,-feat_25_feat_78,-feat_25_feat_45,-feat_93,-feat_25_feat_58,-feat_31,-feat_25_feat_49,-feat_25_feat_57,-feat_61,-feat_28,-feat_25_feat_19,-feat_45,-feat_25_feat_23,-feat_25_feat_69,-feat_7)

new_result <- runAndValidateTraining(data, munged.data$is_class2, 1.0, 9,
                                     "class 2 rf")

current_results <- rbind(current_results, munged.data$result)
train_class4 <- new_result$model

train_class1 <- new_result$model
train_class2 <- new_result$model
train_class3 <- new_result$model
train_class4 <- new_result$model
train_class5 <- new_result$model
train_class6 <- new_result$model
train_class7 <- new_result$model
train_class8 <- new_result$model
train_class9 <- new_result$model

create_submission(train_class1, train_class2, train_class3,
                  train_class4, train_class5, train_class6,
                  train_class7, train_class8,train_class9)

#current_results <- new_result$result

control.config <- trainControl(method = "repeatedcv", repeats = 1,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

rf.grid <- data.frame(.mtry = c(8,9))

logreg.tmp.train <- train(is_class2 ~ .,
                          data = data,
                          method = "rf",
                          metric = "ROC",
                          tuneGrid = rf.grid,
                          trControl = control.config)



class2_valid_pred <- predict(logreg.tmp.train, validation.data) 
confusionMatrix(class2_valid_pred, validation.data$is_class2)

data.frame(feature = rownames(logreg.tmp.train$finalModel$importance), importance = logreg.tmp.train$finalModel$importance) %>% arrange(MeanDecreaseGini)

class2_valid_pred <- predict(new_result$model, validation.data) 

data_vis <- data.frame(validation.data %>% 
                         select(feat_1:feat_93, is_class2), 
                       result = class2_valid_pred) %>%
  mutate(correct = ifelse(is_class2 == result,"Yes", "No")
  )


for (secFeat in 1:93) {
  createResultComparisonPlot(9,secFeat,data_vis,data_vis$is_class2,"RandomForest")
}

ggplot(data_vis, aes(x=feat_9, y=feat_40)) +
  facet_grid(is_class2 ~ correct, labeller = label_both) +
  theme_classic() +
  geom_point(alpha = 0.4) +
  geom_smooth()


data_vis2 <- data.frame(validation.data %>% 
                         select(feat_1:feat_93, is_class2), 
                       result = class2_valid_pred) %>%
  mutate(Actual = ifelse(is_class2 == "Yes", 1, 0),
         Predicted = ifelse(result == "Yes", 1, 0)) %>%
  gather("Type", "Value", Actual, Predicted)

ggplot(data_vis2, aes(x = feat_1, y = Value, color = Type)) +
  #facet_grid(is_class2 ~ correct, labeller = label_both) +
  theme_classic() +
  geom_point(alpha = 0.4) +
  geom_smooth(span = 0.5)

for (feat in 1:93) {
  createResultSmoothPlot(feat, validation.data %>% select(1:93), validation.data$is_class2, class2_valid_pred, "RandomForest")
}


ggplot(data_vis2, aes(x = feat_89, y = Value)) +
  facet_grid(. ~ Type, labeller = label_both) +
  theme_minimal() +
  geom_point(alpha = 0.4) +
  geom_smooth(span = 0.5)


munged.data %>% filter(feat_89 > 15 & is_class2 == "Yes") %>% nrow

min(feat_50, 15)
min(feat_33, 10)
min(feat_10, 10)
min(feat_32, 15)
min(feat_66, 25)
min(feat_70, 20)
min(feat_89, 15)


rf.probs <- predict(new_result$model, newdata = validation.data, type = "prob")
rf.ROC <- roc(response = validation.data$is_class2,
              predictor = rf.probs$Yes,
              levels = levels(validation.data$is_class2))
plot(rf.ROC, col="red") 