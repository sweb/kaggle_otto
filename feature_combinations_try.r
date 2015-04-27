setwd("C:/dev/repositories/R/kaggle_otto")
source("init_ws.r")

i <- 14

feat_9_combinations <- addFeatureCombination(9, munged.data) %>% 
  select(1:93, 181, 116, 171, 180, 122, 177, 94, 154, 168, 126,103, is_class2)
feat_14_combinations <- addFeatureCombination(14, munged.data) %>%
  select(feat_14_feat_1 , feat_14_feat_2 , feat_14_feat_3 , feat_14_feat_8 , feat_14_feat_9 , feat_14_feat_11, feat_14_feat_13, feat_14_feat_15, feat_14_feat_16, feat_14_feat_17, feat_14_feat_23, feat_14_feat_26, feat_14_feat_29, feat_14_feat_30, feat_14_feat_32, feat_14_feat_33, feat_14_feat_34, feat_14_feat_36, feat_14_feat_37, feat_14_feat_38, feat_14_feat_39, feat_14_feat_42, feat_14_feat_43, feat_14_feat_46, feat_14_feat_48, feat_14_feat_51, feat_14_feat_54, feat_14_feat_56, feat_14_feat_59, feat_14_feat_60, feat_14_feat_62, feat_14_feat_66, feat_14_feat_68, feat_14_feat_69, feat_14_feat_72, feat_14_feat_74, feat_14_feat_76, feat_14_feat_77, feat_14_feat_78, feat_14_feat_80, feat_14_feat_82, feat_14_feat_83, feat_14_feat_84, feat_14_feat_87, feat_14_feat_89, feat_14_feat_91, feat_14_feat_92, feat_14_feat_93)
  #select(169,126,172,122,181,121)
feat_25_combinations <- addFeatureCombination(25, munged.data) %>%
  select(103,107,108,111,122,124,126,128,132,137,152,158,161,166,169,170,176,181)
#feat_89_combinations <- normalize(addFeatureCombination(89, munged.data))

feat_40_combinations <- addFeatureCombination(40, munged.data) %>% 
  select(101,104,107,108,110,117,123,126,127,140,152,159,161,166,169,170,181,184,185)

feat_33_combinations <- addFeatureCombination(33, munged.data) %>% 
  select(104,107,108,117,143,154,156,158,164,166,168,171)

feat_15_combinations <- addFeatureCombination(15, munged.data) %>%
  select(102, 103, 104, 108, 116, 122, 123, 125, 126, 130, 134, 135, 140, 151, 154, 155, 159, 163, 164, 165, 171, 172, 176, 178, 181, 184)

feat_72_combinations <- addFeatureCombination(72, munged.data) %>% 
  select(feat_72_feat_9, feat_72_feat_10, feat_72_feat_11, feat_72_feat_16,
         feat_72_feat_19, feat_72_feat_23, feat_72_feat_24, feat_72_feat_25, feat_72_feat_27,
         feat_72_feat_28, feat_72_feat_29, feat_72_feat_33, feat_72_feat_34, feat_72_feat_35,
         feat_72_feat_37, feat_72_feat_38, feat_72_feat_40, feat_72_feat_43, feat_72_feat_44, 
         feat_72_feat_54,feat_72_feat_57, feat_72_feat_58, feat_72_feat_59, feat_72_feat_63,
         feat_72_feat_66, feat_72_feat_67, feat_72_feat_71, feat_72_feat_73, feat_72_feat_74,
         feat_72_feat_76, feat_72_feat_79, feat_72_feat_80,feat_72_feat_83, feat_72_feat_84,
         feat_72_feat_86, feat_72_feat_93)

feat_64_combinations <- addFeatureCombination(64, munged.data) %>% 
  select(feat_64_feat_1 , feat_64_feat_2 , feat_64_feat_7 , feat_64_feat_9 , feat_64_feat_11, feat_64_feat_14, feat_64_feat_19, feat_64_feat_24, feat_64_feat_27, feat_64_feat_29, feat_64_feat_30, feat_64_feat_31, feat_64_feat_32, feat_64_feat_33, feat_64_feat_34, feat_64_feat_36, feat_64_feat_37, feat_64_feat_39, feat_64_feat_40, feat_64_feat_46, feat_64_feat_48, feat_64_feat_50, feat_64_feat_55, feat_64_feat_57, feat_64_feat_58, feat_64_feat_60, feat_64_feat_62, feat_64_feat_63, feat_64_feat_65, feat_64_feat_68, feat_64_feat_70, feat_64_feat_72, feat_64_feat_74, feat_64_feat_75, feat_64_feat_80, feat_64_feat_82, feat_64_feat_83, feat_64_feat_85, feat_64_feat_86, feat_64_feat_88, feat_64_feat_89, feat_64_feat_91, feat_64_feat_93)

feat_88_combinations <- addFeatureCombination(88, munged.data) %>%
  select(feat_88_feat_1 , feat_88_feat_3 , feat_88_feat_7 , feat_88_feat_11, feat_88_feat_14, feat_88_feat_18, feat_88_feat_21, feat_88_feat_22, feat_88_feat_25, feat_88_feat_26, feat_88_feat_27, feat_88_feat_29, feat_88_feat_30, feat_88_feat_33, feat_88_feat_34, feat_88_feat_36, feat_88_feat_39, feat_88_feat_40, feat_88_feat_42, feat_88_feat_45, feat_88_feat_47, feat_88_feat_48, feat_88_feat_50, feat_88_feat_54, feat_88_feat_56, feat_88_feat_59, feat_88_feat_60, feat_88_feat_65, feat_88_feat_69, feat_88_feat_72, feat_88_feat_75, feat_88_feat_78, feat_88_feat_79, feat_88_feat_82, feat_88_feat_86, feat_88_feat_92)

feat_26_combinations <- addFeatureCombination(26, munged.data) %>%
  select(feat_26_feat_1 , feat_26_feat_3 , feat_26_feat_34, feat_26_feat_40, feat_26_feat_48, feat_26_feat_49, feat_26_feat_51, feat_26_feat_53, feat_26_feat_56, feat_26_feat_58, feat_26_feat_60, feat_26_feat_62, feat_26_feat_63, feat_26_feat_64, feat_26_feat_66, feat_26_feat_69, feat_26_feat_71, feat_26_feat_80, feat_26_feat_93)

set.seed(42)
# feat_9_combinations <- normalize(addFeatureCombination(9, munged.data)) %>% 
#   select(1:93, 181, 116, 171, 180, 122, 177, 94, 154, 168, 126,103, is_class2)
# feat_14_combinations <- normalize(addFeatureCombination(14, munged.data)) %>%
#   select(169,126,172,122,181,121)
# feat_25_combinations <- normalize(addFeatureCombination(25, munged.data)) %>%
#   select(103,107,108,111,122,124,126,128,132,137,152,158,161,166,169,170,176,181)
# #feat_89_combinations <- normalize(addFeatureCombination(89, munged.data))
# 
# feat_40_combinations <- normalize(addFeatureCombination(40, munged.data)) %>% 
#   select(101,104,107,108,110,117,123,126,127,140,152,159,161,166,169,170,181,184,185)
# 
# feat_33_combinations <- normalize(addFeatureCombination(33, munged.data)) %>% 
#   select(104,107,108,117,143,154,156,158,164,166,168,171)
# 
# feat_15_combinations <- normalize(addFeatureCombination(15, munged.data)) %>% 
#   select(102, 103, 104, 108, 116, 117, 122, 123, 125, 126, 130, 134, 135, 140, 151, 154, 155, 159, 163, 164, 165, 171, 172, 176, 178, 181, 184)

#next try: 88
# 32
engineered.data <- data.frame(feat_9_combinations, feat_14_combinations, 
                              feat_25_combinations, feat_40_combinations, 
                              feat_15_combinations, feat_72_combinations,
                              feat_64_combinations, feat_88_combinations)

engineered.data <- normalize(addFeatureCombination(14, munged.data)) #%>% 
   #select(feat_1:feat_93,feat_26_feat_1 , feat_26_feat_3 , feat_26_feat_34, feat_26_feat_40, feat_26_feat_48, feat_26_feat_49, feat_26_feat_51, feat_26_feat_53, feat_26_feat_56, feat_26_feat_58, feat_26_feat_60, feat_26_feat_62, feat_26_feat_63, feat_26_feat_64, feat_26_feat_66, feat_26_feat_69, feat_26_feat_71, feat_26_feat_80, feat_26_feat_93, is_class2)


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

#current_results <- data.frame(train = confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)$overall[1], valid = confusionMatrix(logreg.tmp.pred, v.data$is_class2)$overall[1])
current_results <- rbind(current_results, data.frame(descr = "Additional feature 14 combinations",train = confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)$overall[1], valid = confusionMatrix(logreg.tmp.pred, v.data$is_class2)$overall[1]))

logreg.tmp.test <- predict(logreg.tmp.train, ts.data)
confusionMatrix(logreg.tmp.test, ts.data$is_class2)$overall[1]