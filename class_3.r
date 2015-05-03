setwd("C:/dev/repositories/R/kaggle_otto")
source("init_ws.r")

require(reshape2)

engineered.data <- data.frame(normalize(munged.data %>% select(1:93)), munged.data %>% select(is_class3))

set.seed(42)
split.data <- createDataPartition(engineered.data$is_class3, p = 0.6, list = FALSE)
t.data <- engineered.data[split.data, ]
tmp.test.data <- engineered.data[-split.data, ]
set.seed(42)
split.test <- createDataPartition(tmp.test.data$is_class3, p = 0.5, list = FALSE)

v.data <- tmp.test.data[split.test,]
ts.data <- tmp.test.data[-split.test,]

control.config <- trainControl(method = "repeatedcv", repeats = 1,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

logreg.tmp.train <- train(is_class3 ~ .,
                          data = t.data %>% select(feat_1:feat_93, is_class3),
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred <- predict(logreg.tmp.train, v.data)
tmp.val.acc <- confusionMatrix(logreg.tmp.pred, v.data$is_class3)

logreg.tmp.prob <- predict(logreg.tmp.train, v.data, type="prob")

logreg.tmp.pred_train <- predict(logreg.tmp.train, t.data)
tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, t.data$is_class3)



comp <- data.frame(result = logreg.tmp.pred, prob = logreg.tmp.prob, v.data)
correct_class <- comp %>% filter(result == is_class3) %>% 
  filter(result == "Yes")

correct_class.no <- comp %>% filter(result == is_class3) %>% 
  filter(result == "No")

wrong_class.yes <- comp %>% filter(result != is_class3) %>% 
  filter(result == "Yes")

wrong_class.no <- comp %>% filter(result != is_class3) %>% 
  filter(result == "No")


multiplyCoefficients <- function(coeff, data) {
  res <-t(t(cbind(1, data %>% select(feat_1:feat_93))) * coeff)
  return (res)
}


avg_feat_correct_class <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, correct_class) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_correct_class.no <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, correct_class.no) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_wrong_class.no <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, wrong_class.no) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_validation <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, v.data) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

avg_feat_wrong_class.yes <- multiplyCoefficients(logreg.tmp.train$finalModel$coefficients, wrong_class.yes) %>% 
  data.frame %>% summarise_each(funs(mean)) %>% melt

joined_data <- inner_join(avg_feat_correct_class, avg_feat_wrong_class.no, by="variable") %>%
  inner_join(avg_feat_validation, by="variable") %>%
  rename(Correct = value.x, Wrong = value.y, Validation = value) %>%
  inner_join(avg_feat_correct_class.no, by="variable") %>%
  rename(Correct_No = value)

difference <- joined_data %>% 
  mutate(diff = abs(Correct - Wrong)) %>% top_n(15, diff) %>% arrange(desc(diff))


gather_diff <- difference %>% select(-diff) %>% gather("class", "value", 2:5)

#write.csv(gather_diff, file = "observations/class2_diff_features.csv", quote = TRUE, row.names = TRUE)

ggplot(data = gather_diff, aes(x=reorder(variable, value))) +
  geom_bar(stat="identity", position="dodge", aes(y=value,fill=class)) +
  theme_minimal() +
  scale_fill_brewer(type="div", palette = "RdBu") +
  coord_flip()

ggsave(file="plot.png", width = 20, height = 20)

for (feat in 1:93) {
  for (i in 1:feat) {
    createComparisonPlot(feat, i, train.data)
  }
}

for (i in 1:93) {
  createComparisonPlot(43, i, train.data, train.data$is_class3)
}

relevant_features <- list(5, 8, 9, 10, 11, 14, 15, 16, 20, 25, 33, 37, 38, 40, 44, 54, 56, 62, 70, 72, 80, 85, 86, 88, 89)

results <- list()
relevant_features <- list(5, 8, 9, 10, 11, 14, 15, 16, 20, 25, 33, 37, 38, 40, 44, 54, 56, 62, 70, 72, 80, 85, 86, 88, 89)
length(results) <- length(relevant_features)
counter <- 1
for (i in relevant_features) {
  a <- Sys.time()
  print(i)
  results[[counter]] <- evaluateFeaturePolynoms(43,i, 
                                                data.frame(train.data %>% select(1:93), 
                                                           is_class = train.data$is_class3), 
                                                data.frame(validation.data %>% select(1:93), 
                                                           is_class = validation.data$is_class3),5)
  counter <- counter + 1
  print (Sys.time() - a)
}

base_data_feat_43 <- addFeatureCombination(40, munged.data %>% select(1:93))

neg_data <- munged.data %>% select(1:93) %>%
  select(-feat_20, -feat_10, -feat_65, -feat_63, -feat_55, -feat_27, -feat_80, -feat_46, -feat_35, -feat_59, -feat_13, -feat_4, -feat_3, -feat_17, -feat_68, -feat_29, -feat_69, -feat_12, -feat_52, -feat_90, -feat_30, -feat_79, -feat_57, -feat_58, -feat_23, -feat_19, -feat_49, -feat_77, -feat_47, -feat_78, -feat_31, -feat_7, -feat_2, -feat_45, -feat_28, -feat_5, -feat_91, -feat_93, -feat_61, -feat_82, -feat_81, -feat_6, -feat_84, -feat_51) %>%
  select(-feat_11, -feat_89, -feat_76, -feat_44, -feat_92, -feat_50, -feat_18, -feat_41, -feat_75, -feat_87, -feat_73, -feat_26, -feat_39, -feat_83) %>%
  mutate(
    #neg_60 = ifelse(feat_60 <= 10, 1, 0), 
         feat_43_neg_60 = ifelse(feat_43 >= 17, 1, 0)*ifelse(feat_60 <= 1, 1, 0),
         feat_32_neg_60 = feat_32*ifelse(feat_60 <= 10, 1, 0))

class2_data <- munged.data %>% select(1:93) %>%
  select(-feat_27, -feat_10, -feat_39, -feat_58, -feat_29, -feat_90, -feat_63, -feat_69, -feat_65, -feat_30, -feat_46, -feat_78, -feat_12, -feat_77, -feat_52, -feat_23, -feat_91, -feat_19, -feat_2, -feat_49, -feat_28, -feat_5, -feat_47, -feat_61, -feat_7, -feat_45, -feat_93, -feat_31, -feat_82, -feat_81, -feat_6, -feat_84, -feat_51) %>%
  mutate(feat_14_feat_48 = feat_14 * feat_48,
         feat_14_feat_15 = feat_14 * feat_48 * feat_40 )

class2_data <- addFeatureCombination(14, munged.data %>% select(1:93)) %>%
  select(-feat_14_feat_70, -feat_14_feat_64, -feat_20, -feat_22, -feat_80, -feat_18, -feat_57, -feat_53, -feat_76, -feat_13, -feat_71, -feat_79, -feat_59, -feat_44, -feat_55, -feat_1, -feat_41, -feat_89, -feat_50, -feat_83, -feat_14_feat_54, -feat_68, -feat_14_feat_85, -feat_35, -feat_74, -feat_92, -feat_56, -feat_87, -feat_73, -feat_14_feat_53, -feat_3, -feat_37, -feat_14_feat_42, -feat_14_feat_36, -feat_14_feat_32, -feat_4, -feat_27, -feat_10, -feat_21, -feat_14_feat_92, -feat_90, -feat_39, -feat_29, -feat_14_feat_1, -feat_69, -feat_14_feat_41, -feat_14_feat_22, -feat_14_feat_38, -feat_58, -feat_14_feat_9, -feat_63, -feat_14_feat_66, -feat_14_feat_26, -feat_14_feat_34, -feat_14_feat_8, -feat_14_feat_55, -feat_78, -feat_14_feat_29, -feat_14_feat_75, -feat_46, -feat_65, -feat_14_feat_56, -feat_14_feat_76, -feat_14_feat_44, -feat_14_feat_89, -feat_14_feat_87, -feat_14_feat_13, -feat_14_feat_17, -feat_14_feat_18, -feat_14_feat_21, -feat_30, -feat_12, -feat_14_feat_20, -feat_52, -feat_14_feat_74, -feat_14_feat_79, -feat_14_feat_10, -feat_49, -feat_77, -feat_14_feat_59, -feat_14_feat_60, -feat_91, -feat_14_feat_11, -feat_14_feat_50, -feat_14_feat_68, -feat_19, -feat_14_feat_65, -feat_14_feat_37, -feat_23, -feat_14_feat_71, -feat_28, -feat_14_feat_39, -feat_2, -feat_14_feat_35, -feat_61, -feat_14_feat_77, -feat_47, -feat_14_feat_73, -feat_14_feat_27, -feat_5, -feat_14_feat_80, -feat_7, -feat_14_feat_46, -feat_14_feat_3, -feat_14_feat_52, -feat_14_feat_63, -feat_31, -feat_14_feat_90, -feat_14_feat_12, -feat_93, -feat_14_feat_4, -feat_45, -feat_14_feat_30, -feat_14_feat_19, -feat_14_feat_58, -feat_14_feat_69, -feat_14_feat_49, -feat_14_feat_5, -feat_14_feat_91, -feat_82, -feat_14_feat_83, -feat_14_feat_45, -feat_14_feat_23, -feat_81, -feat_14_feat_78, -feat_14_feat_57, -feat_14_feat_7, -feat_14_feat_28, -feat_6, -feat_14_feat_93, -feat_14_feat_31, -feat_14_feat_2, -feat_14_feat_47, -feat_14_feat_6, -feat_14_feat_81, -feat_14_feat_82, -feat_84, -feat_14_feat_61, -feat_51, -feat_14_feat_84, -feat_14_feat_51)

new_result <- runAndValidateTraining(class2_data, munged.data$is_class2, 0.2,
                                     "class2 sig <0.15 removed")

current_results <- rbind(current_results, new_result$result)
current_results

class2_valid_pred <- predict(new_result$model, validation.data) 

data_vis <- data.frame(validation.data %>% 
                         select(feat_1:feat_93, is_class2), 
                       result = class2_valid_pred) %>%
  mutate(correct_yes = ifelse(is_class2 == result & is_class2 == "Yes", 1, 0),
         correct_no = ifelse(is_class2 == result & is_class2 == "No", 1, 0),
         wrong_yes = ifelse(is_class2 != result & is_class2 == "No", 1, 0),
         result_type = ifelse(correct_yes == 1, "Correct Yes",
                              ifelse(correct_no == 1, "Correct No",
                                     ifelse(wrong_yes == 1, "Wrong Yes","Wrong No"))),
         result_type = as.factor(result_type)
  ) %>%
  select(-correct_yes, -correct_no, -wrong_yes)

summary(data_vis)

ggplot(data_vis  %>% 
         filter(result_type == "Correct Yes" | result_type == "Wrong Yes") %>%
         mutate(feat_14_feat_48 = feat_14 * feat_48 * feat_40), aes(x=feat_14_feat_48, y=feat_15, color=result_type)) +
  theme_classic() +
  geom_point(alpha = 0.4)