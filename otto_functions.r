

munge_data <- function(data) {
  munged.data <- data %>%  select(-id) %>%
    mutate(is_class1 = ifelse(target == 'Class_1', 'Yes', 'No'), 
           is_class2 = ifelse(target == 'Class_2', 'Yes', 'No'),
           is_class3 = ifelse(target == 'Class_3', 'Yes', 'No'),
           is_class4 = ifelse(target == 'Class_4', 'Yes', 'No'),
           is_class5 = ifelse(target == 'Class_5', 'Yes', 'No'),
           is_class6 = ifelse(target == 'Class_6', 'Yes', 'No'),
           is_class7 = ifelse(target == 'Class_7', 'Yes', 'No'),
           is_class8 = ifelse(target == 'Class_8', 'Yes', 'No'),
           is_class9 = ifelse(target == 'Class_9', 'Yes', 'No')) %>%
    select(-target)
  
  polished.data <- munged.data %>% mutate(is_class1 = as.factor(is_class1),
                                          is_class2 = as.factor(is_class2),
                                          is_class3 = as.factor(is_class3),
                                          is_class4 = as.factor(is_class4),
                                          is_class5 = as.factor(is_class5),
                                          is_class6 = as.factor(is_class6),
                                          is_class7 = as.factor(is_class7),
                                          is_class8 = as.factor(is_class8),
                                          is_class9 = as.factor(is_class9))
  return (polished.data)
}




predict_data <- function(data, data2,
                         train.class1, train.class2, train.class3, 
                         train.class4, train.class5, train.class6, 
                         train.class7, train.class8, train.class9) {
  
  prob.class1 <- predict(train.class1, data, type="prob")
  prob.class2 <- predict(train.class2, data2, type="prob")
  prob.class3 <- predict(train.class3, data, type="prob")
  prob.class4 <- predict(train.class4, data, type="prob")
  prob.class5 <- predict(train.class5, data, type="prob")
  prob.class6 <- predict(train.class6, data, type="prob")
  prob.class7 <- predict(train.class7, data, type="prob")
  prob.class8 <- predict(train.class8, data, type="prob")
  prob.class9 <- predict(train.class9, data, type="prob")
  
  probs <- cbind(prob.class1$Yes,
                 prob.class2$Yes,
                 prob.class3$Yes,
                 prob.class4$Yes,
                 prob.class5$Yes,
                 prob.class6$Yes,
                 prob.class7$Yes,
                 prob.class8$Yes,
                 prob.class9$Yes) %>% round(6) %>% format(nsmall = 6)
  
  colnames(probs) <- c("Class_1", "Class_2", "Class_3", "Class_4", "Class_5",
                       "Class_6", "Class_7", "Class_8", "Class_9")
  
  tmp.probs <- data.frame(probs)
  tmp.probs[, "max"] <- apply(tmp.probs[,1:9],1,max)
  
  pred <- tmp.probs %>% mutate(Class_1 = ifelse(Class_1 == max, 1, 0),
                               Class_2 = ifelse(Class_2 == max, 1, 0),
                               Class_3 = ifelse(Class_3 == max, 1, 0),
                               Class_4 = ifelse(Class_4 == max, 1, 0),
                               Class_5 = ifelse(Class_5 == max, 1, 0),
                               Class_6 = ifelse(Class_6 == max, 1, 0),
                               Class_7 = ifelse(Class_7 == max, 1, 0),
                               Class_8 = ifelse(Class_8 == max, 1, 0),
                               Class_9 = ifelse(Class_9 == max, 1, 0)) %>% select(-max)
  
  res <- cbind( id = data$id, probs)
  return (res)
}


create_submission <- function(train.class1, train.class2, train.class3, 
                              train.class4, train.class5, train.class6, 
                              train.class7, train.class8, train.class9) {
  data <- read.csv("data/test.csv", header = TRUE, sep = ",", na.strings = "")
  
  data2 <- engineerDataClass2(data %>% select(-id))
  
  predictions <- predict_data(data, data2, train.class1, train.class2, train.class3, 
                              train.class4, train.class5, train.class6, 
                              train.class7, train.class8, train.class9)
  print(summary(predictions))
  write.csv(predictions, file = "result/submission.csv", 
            quote = FALSE, row.names = FALSE)
}


normalize <- function(df) {
  mat <- data.matrix(df %>% select(-(is_class2)))
  norm_mat <- apply(mat, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
  data <- data.frame(norm_mat, df %>% select(is_class2))
  return (data)
}

normalize2 <- function(df) {
  mat <- data.matrix(df)
  norm_mat <- apply(mat, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
  data <- data.frame(norm_mat)
  return (data)
}


addFeatureCombination <- function(featureId, data) {
  new_feat <- data[,featureId] * select(data, feat_1:feat_93, -featureId)
  colnames(new_feat) <- paste(colnames(select(data, featureId)),colnames(new_feat), sep="_")
  
  new_data <- data.frame(data %>% select(-(is_class1:is_class9)), new_feat, is_class2 = data$is_class2)
  return (new_data)
}

addFeatureCombination2 <- function(featureId, data) {
  new_feat <- data[,featureId] * select(data, feat_1:feat_93, -featureId)
  colnames(new_feat) <- paste(colnames(select(data, featureId)),colnames(new_feat), sep="_")
  
  new_data <- data.frame(data, new_feat)
  return (new_data)
}

engineerDataClass2 <- function(data) {
  feat_9_combinations <- addFeatureCombination2(9, data) %>% 
    select(1:93, 181, 116, 171, 180, 122, 177, 94, 154, 168, 126,103)
  feat_14_combinations <- addFeatureCombination2(14, data) %>%
    select(feat_14_feat_1 , feat_14_feat_2 , feat_14_feat_3 , feat_14_feat_8 , feat_14_feat_9 , feat_14_feat_11, feat_14_feat_13, feat_14_feat_15, feat_14_feat_16, feat_14_feat_17, feat_14_feat_23, feat_14_feat_26, feat_14_feat_29, feat_14_feat_30, feat_14_feat_32, feat_14_feat_33, feat_14_feat_34, feat_14_feat_36, feat_14_feat_37, feat_14_feat_38, feat_14_feat_39, feat_14_feat_42, feat_14_feat_43, feat_14_feat_46, feat_14_feat_48, feat_14_feat_51, feat_14_feat_54, feat_14_feat_56, feat_14_feat_59, feat_14_feat_60, feat_14_feat_62, feat_14_feat_66, feat_14_feat_68, feat_14_feat_69, feat_14_feat_72, feat_14_feat_74, feat_14_feat_76, feat_14_feat_77, feat_14_feat_78, feat_14_feat_80, feat_14_feat_82, feat_14_feat_83, feat_14_feat_84, feat_14_feat_87, feat_14_feat_89, feat_14_feat_91, feat_14_feat_92, feat_14_feat_93)
  #select(169,126,172,122,181,121)
  feat_25_combinations <- addFeatureCombination2(25, data) %>%
    select(103,107,108,111,122,124,126,128,132,137,152,158,161,166,169,170,176,181)
  #feat_89_combinations <- normalize(addFeatureCombination(89, munged.data))
  
  feat_40_combinations <- addFeatureCombination2(40, data) %>% 
    select(101,104,107,108,110,117,123,126,127,140,152,159,161,166,169,170,181,184,185)
  
  feat_33_combinations <- addFeatureCombination2(33, data) %>% 
    select(104,107,108,117,143,154,156,158,164,166,168,171)
  
  feat_15_combinations <- addFeatureCombination2(15, data) %>%
    select(102, 103, 104, 108, 116, 122, 123, 125, 126, 130, 134, 135, 140, 151, 154, 155, 159, 163, 164, 165, 171, 172, 176, 178, 181, 184)
  
  feat_72_combinations <- addFeatureCombination2(72, data) %>% 
    select(feat_72_feat_9, feat_72_feat_10, feat_72_feat_11, feat_72_feat_16,
           feat_72_feat_19, feat_72_feat_23, feat_72_feat_24, feat_72_feat_25, feat_72_feat_27,
           feat_72_feat_28, feat_72_feat_29, feat_72_feat_33, feat_72_feat_34, feat_72_feat_35,
           feat_72_feat_37, feat_72_feat_38, feat_72_feat_40, feat_72_feat_43, feat_72_feat_44, 
           feat_72_feat_54,feat_72_feat_57, feat_72_feat_58, feat_72_feat_59, feat_72_feat_63,
           feat_72_feat_66, feat_72_feat_67, feat_72_feat_71, feat_72_feat_73, feat_72_feat_74,
           feat_72_feat_76, feat_72_feat_79, feat_72_feat_80,feat_72_feat_83, feat_72_feat_84,
           feat_72_feat_86, feat_72_feat_93)
  
  feat_64_combinations <- addFeatureCombination2(64, data) %>% 
    select(feat_64_feat_1 , feat_64_feat_2 , feat_64_feat_7 , feat_64_feat_9 , feat_64_feat_11, feat_64_feat_14, feat_64_feat_19, feat_64_feat_24, feat_64_feat_27, feat_64_feat_29, feat_64_feat_30, feat_64_feat_31, feat_64_feat_32, feat_64_feat_33, feat_64_feat_34, feat_64_feat_36, feat_64_feat_37, feat_64_feat_39, feat_64_feat_40, feat_64_feat_46, feat_64_feat_48, feat_64_feat_50, feat_64_feat_55, feat_64_feat_57, feat_64_feat_58, feat_64_feat_60, feat_64_feat_62, feat_64_feat_63, feat_64_feat_65, feat_64_feat_68, feat_64_feat_70, feat_64_feat_72, feat_64_feat_74, feat_64_feat_75, feat_64_feat_80, feat_64_feat_82, feat_64_feat_83, feat_64_feat_85, feat_64_feat_86, feat_64_feat_88, feat_64_feat_89, feat_64_feat_91, feat_64_feat_93)
  
  feat_88_combinations <- addFeatureCombination2(88, data) %>%
    select(feat_88_feat_1 , feat_88_feat_3 , feat_88_feat_7 , feat_88_feat_11, feat_88_feat_14, feat_88_feat_18, feat_88_feat_21, feat_88_feat_22, feat_88_feat_25, feat_88_feat_26, feat_88_feat_27, feat_88_feat_29, feat_88_feat_30, feat_88_feat_33, feat_88_feat_34, feat_88_feat_36, feat_88_feat_39, feat_88_feat_40, feat_88_feat_42, feat_88_feat_45, feat_88_feat_47, feat_88_feat_48, feat_88_feat_50, feat_88_feat_54, feat_88_feat_56, feat_88_feat_59, feat_88_feat_60, feat_88_feat_65, feat_88_feat_69, feat_88_feat_72, feat_88_feat_75, feat_88_feat_78, feat_88_feat_79, feat_88_feat_82, feat_88_feat_86, feat_88_feat_92)
  
  feat_26_combinations <- addFeatureCombination2(26, data) %>%
    select(feat_26_feat_1 , feat_26_feat_3 , feat_26_feat_34, feat_26_feat_40, feat_26_feat_48, feat_26_feat_49, feat_26_feat_51, feat_26_feat_53, feat_26_feat_56, feat_26_feat_58, feat_26_feat_60, feat_26_feat_62, feat_26_feat_63, feat_26_feat_64, feat_26_feat_66, feat_26_feat_69, feat_26_feat_71, feat_26_feat_80, feat_26_feat_93)
  
  engineered.data <- data.frame(feat_9_combinations, feat_14_combinations, 
                                feat_25_combinations, feat_40_combinations, 
                                feat_15_combinations, feat_72_combinations,
                                feat_64_combinations, feat_88_combinations)
  return(engineered.data)
}