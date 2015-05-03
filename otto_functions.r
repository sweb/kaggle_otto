

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
  mat <- data.matrix(df)
  norm_mat <- apply(mat, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
  data <- data.frame(norm_mat)
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
  
  new_data <- data.frame(data, new_feat)
  return (new_data)
}

addFeatureCombination2 <- function(featureId, data) {
  new_feat <- data[,featureId] * select(data, feat_1:feat_93, -featureId)
  colnames(new_feat) <- paste(colnames(select(data, featureId)),colnames(new_feat), sep="_")
  
  new_data <- data.frame(data, new_feat)
  return (new_data)
}

engineerDataClass2 <- function(data) {
  base_data_feat_14 <- addFeatureCombination2(14, data)
  
  relevant_features_14 <- list(15,72,9,16,43,33,34,38,48,59,60,62,66,77,89,92,40)
  degree_list_14 <- list(3,4,3,3,3,3,3,4,3,3,3,3,2,3,3,3,3)
  poly_14_feat <- createAllPolyFeatures(data, 14, relevant_features_14, degree_list_14)
  
  poly_14_data <- data.frame(base_data_feat_14, poly_14_feat) %>% select(-contains(".")) %>% select(-feat_14_feat_3, -feat_14_feat_4, -feat_14_feat_5, -feat_14_feat_6, -feat_14_feat_7, -feat_14_feat_10, -feat_14_feat_12, -feat_14_feat_13, -feat_14_feat_16, -feat_14_feat_17, -feat_14_feat_18, -feat_14_feat_19, -feat_14_feat_20, -feat_14_feat_21, -feat_14_feat_23, -feat_14_feat_27, -feat_14_feat_28, -feat_14_feat_31, -feat_14_feat_33, -feat_14_feat_35, -feat_14_feat_37, -feat_14_feat_45, -feat_14_feat_48, -feat_14_feat_50, -feat_14_feat_52, -feat_14_feat_55, -feat_14_feat_57, -feat_14_feat_58, -feat_14_feat_61, -feat_14_feat_62, -feat_14_feat_65, -feat_14_feat_67, -feat_14_feat_70, -feat_14_feat_71, -feat_14_feat_73, -feat_14_feat_74, -feat_14_feat_75, -feat_14_feat_77, -feat_14_feat_78, -feat_14_feat_79, -feat_14_feat_81, -feat_14_feat_82, -feat_14_feat_85, -feat_14_feat_86, -feat_14_feat_89, -feat_14_feat_91, -feat_72e2, -feat_14e2_feat_72, -feat_72e3, -feat_14e4, -feat_14e3_feat_72, -feat_16e2, -feat_14_feat_16e2, -feat_16e3, -feat_43e2, -feat_14e2_feat_43, -feat_14_feat_33e2, -feat_34e2, -feat_34e3, -feat_38e2, -feat_38e3, -feat_14e3_feat_38, -feat_14_feat_38e3, -feat_38e4, -feat_14_feat_48e2, -feat_60e2, -feat_14e2_feat_60, -feat_14_feat_60e2, -feat_60e3, -feat_14_feat_62e2, -feat_14e2_feat_77, -feat_14_feat_77e2, -feat_14_feat_89e2, -feat_92e2, -feat_14e2_feat_92, -feat_14_feat_92e2, -feat_92e3)
  
  base_data_feat_9 <- addFeatureCombination2(9, data)
  
  relevant_features_9 <- list(6, 10, 14, 15, 20, 48, 50, 54, 56, 62, 64, 66, 70, 77, 79, 88)
  degree_list_9 <- list(3,3,3,3,2,2,3,3,4,4,4,2,3,3,4,4)
  
  poly_9_feat <- createAllPolyFeatures(data, 9, relevant_features_9, degree_list_9)
  
  poly_9_data <- data.frame(base_data_feat_9, poly_9_feat) %>% select(-contains(".")) %>% select(-feat_9_feat_2, -feat_9_feat_3, -feat_9_feat_4, -feat_9_feat_5, -feat_9_feat_6, -feat_9_feat_8, -feat_9_feat_10, -feat_9_feat_12, -feat_9_feat_13, -feat_9_feat_15, -feat_9_feat_16, -feat_9_feat_18, -feat_9_feat_20, -feat_9_feat_22, -feat_9_feat_23, -feat_9_feat_25, -feat_9_feat_28, -feat_9_feat_31, -feat_9_feat_33, -feat_9_feat_35, -feat_9_feat_37, -feat_9_feat_40, -feat_9_feat_41, -feat_9_feat_42, -feat_9_feat_44, -feat_9_feat_47, -feat_9_feat_49, -feat_9_feat_50, -feat_9_feat_51, -feat_9_feat_52, -feat_9_feat_56, -feat_9_feat_59, -feat_9_feat_61, -feat_9_feat_62, -feat_9_feat_65, -feat_9_feat_66, -feat_9_feat_69, -feat_9_feat_70, -feat_9_feat_71, -feat_9_feat_73, -feat_9_feat_77, -feat_9_feat_80, -feat_9_feat_81, -feat_9_feat_83, -feat_9_feat_84, -feat_9_feat_85, -feat_9_feat_86, -feat_9_feat_87, -feat_9_feat_90, -feat_9_feat_91, -feat_9_feat_92, -feat_9_feat_93, -feat_6e2, -feat_9e3, -feat_9e2_feat_6, -feat_9_feat_6e2, -feat_6e3, -feat_10e2, -feat_9e2_feat_10, -feat_9_feat_10e2, -feat_10e3, -feat_9e2_feat_15, -feat_20e2, -feat_50e2, -feat_9_feat_50e2, -feat_50e3, -feat_56e2, -feat_9e2_feat_56, -feat_9_feat_56e2, -feat_56e3, -feat_9e4, -feat_9e3_feat_56, -feat_9e2_feat_56e2, -feat_9_feat_56e3, -feat_56e4, -feat_9e2_feat_62, -feat_9_feat_62e2, -feat_9e3_feat_62, -feat_9e2_feat_62e2, -feat_9_feat_62e3, -feat_62e4, -feat_9e2_feat_64, -feat_9e3_feat_64, -feat_64e4, -feat_9_feat_70e2, -feat_9e2_feat_77, -feat_9_feat_77e2, -feat_79e3, -feat_79e4, -feat_88e2, -feat_88e3, -feat_9_feat_88e3)
  
  base_data_feat_25 <- addFeatureCombination2(25, data)
  
  relevant_features_25 <- list(14, 15, 16, 40, 42, 44, 48, 54, 66, 67, 72, 88)
  degree_list_25 <- list(3,4,3,4,4,3,4,3,3,5,4,4)
  
  poly_25_feat <- createAllPolyFeatures(data, 25, relevant_features_25, degree_list_25)
  
  poly_25_data <- data.frame(base_data_feat_25, poly_25_feat) %>% select(-contains(".")) %>% select(-feat_25_feat_2, -feat_25_feat_4, -feat_25_feat_6, -feat_25_feat_7, -feat_25_feat_9, -feat_25_feat_10, -feat_25_feat_12, -feat_25_feat_16, -feat_25_feat_17, -feat_25_feat_21, -feat_25_feat_22, -feat_25_feat_26, -feat_25_feat_27, -feat_25_feat_28, -feat_25_feat_33, -feat_25_feat_35, -feat_25_feat_37, -feat_25_feat_42, -feat_25_feat_43, -feat_25_feat_46, -feat_25_feat_47, -feat_25_feat_48, -feat_25_feat_50, -feat_25_feat_51, -feat_25_feat_52, -feat_25_feat_53, -feat_25_feat_55, -feat_25_feat_57, -feat_25_feat_58, -feat_25_feat_59, -feat_25_feat_61, -feat_25_feat_63, -feat_25_feat_64, -feat_25_feat_66, -feat_25_feat_68, -feat_25_feat_70, -feat_25_feat_71, -feat_25_feat_73, -feat_25_feat_75, -feat_25_feat_76, -feat_25_feat_79, -feat_25_feat_80, -feat_25_feat_81, -feat_25_feat_82, -feat_25_feat_87, -feat_25_feat_90, -feat_25_feat_91, -feat_25_feat_93, -feat_25e3, -feat_25e2_feat_14, -feat_25e4, -feat_25e2_feat_16, -feat_16e3, -feat_25_feat_40e2, -feat_25e3_feat_40, -feat_25_feat_40e3, -feat_25e2_feat_42e2, -feat_25_feat_42e3, -feat_42e4, -feat_44e2, -feat_25_feat_44e2, -feat_44e3, -feat_48e2, -feat_25e2_feat_48, -feat_48e3, -feat_48e4, -feat_25e2_feat_54, -feat_25e2_feat_66, -feat_25_feat_66e2, -feat_25e2_feat_67, -feat_25_feat_67e3, -feat_25e2_feat_67e3, -feat_25_feat_67e4, -feat_72e2, -feat_25e2_feat_72, -feat_72e3, -feat_25e2_feat_72e2)
  
  base_data_feat_40 <- addFeatureCombination2(40, data) #munged.data %>% select(1:93, is_class2)
  
  relevant_features_40 <- list(15,18,44,48,54,62,66,67,70,79,89)
  degree_list_40 <- list(4,3,4,4,3,3,4,3,4,4,3)
  
  poly_40_feat <- createAllPolyFeatures(data, 40, relevant_features_40, degree_list_40)
  
  poly_40_data <- data.frame(base_data_feat_40, poly_40_feat) %>% 
    select(-contains(".")) %>% select(-feat_2, -feat_4, -feat_7, -feat_10, -feat_21, -feat_28, -feat_31, -feat_35, -feat_37, -feat_38, -feat_44, -feat_45, -feat_49, -feat_52, -feat_55, -feat_65, -feat_70, -feat_74, -feat_76, -feat_82, -feat_83, -feat_86, -feat_87, -feat_89, -feat_93, -feat_40_feat_18, -feat_18e2, -feat_40e2_feat_18, -feat_18e3, -feat_44e2, -feat_40_feat_44e2, -feat_44e3, -feat_40e3_feat_44, -feat_40e2_feat_44e2, -feat_40_feat_44e3, -feat_44e4, -feat_48e3, -feat_48e4, -feat_40_feat_62, -feat_40e2_feat_62, -feat_40_feat_62e2, -feat_40_feat_66, -feat_40e2_feat_66, -feat_40_feat_66e2, -feat_66e3, -feat_40e2_feat_66e2, -feat_66e4, -feat_40_feat_70, -feat_70e2, -feat_40e2_feat_70, -feat_40_feat_70e2, -feat_70e3, -feat_40e2_feat_70e2, -feat_40_feat_70e3, -feat_70e4, -feat_79e2, -feat_79e3, -feat_40_feat_79e3, -feat_79e4, -feat_40_feat_89, -feat_89e2, -feat_40_feat_89e2, -feat_89e3)
  
  base_data_feat_15 <- addFeatureCombination2(15, data)# %>% select(1:93, is_class2)
  relevant_features_15 <- list(16,21,48,54,62,64,66,67,70,85,88)
  degree_list_15 <- list(3,4,3,3,3,3,3,4,3,3,4)
  poly_15_feat <- createAllPolyFeatures(data, 15, relevant_features_15, degree_list_15)
  poly_15_data <- data.frame(base_data_feat_15, poly_15_feat) %>% select(-contains(".")) %>%
    select(-feat_1, -feat_3, -feat_4, -feat_5, -feat_7, -feat_10, -feat_12, -feat_18, -feat_24, -feat_37, -feat_43, -feat_44, -feat_45, -feat_49, -feat_52, -feat_54, -feat_74, -feat_76, -feat_81, -feat_82, -feat_83, -feat_93, -feat_15_feat_2, -feat_15_feat_3, -feat_15_feat_4, -feat_15_feat_5, -feat_15_feat_6, -feat_15_feat_7, -feat_15_feat_12, -feat_15_feat_13, -feat_15_feat_14, -feat_15_feat_18, -feat_15_feat_19, -feat_15_feat_20, -feat_15_feat_21, -feat_15_feat_22, -feat_15_feat_26, -feat_15_feat_27, -feat_15_feat_29, -feat_15_feat_32, -feat_15_feat_35, -feat_15_feat_36, -feat_15_feat_41, -feat_15_feat_44, -feat_15_feat_46, -feat_15_feat_47, -feat_15_feat_49, -feat_15_feat_50, -feat_15_feat_51, -feat_15_feat_52, -feat_15_feat_53, -feat_15_feat_55, -feat_15_feat_56, -feat_15_feat_58, -feat_15_feat_60, -feat_15_feat_61, -feat_15_feat_62, -feat_15_feat_64, -feat_15_feat_65, -feat_15_feat_66, -feat_15_feat_68, -feat_15_feat_69, -feat_15_feat_70, -feat_15_feat_74, -feat_15_feat_75, -feat_15_feat_76, -feat_15_feat_77, -feat_15_feat_78, -feat_15_feat_81, -feat_15_feat_82, -feat_15_feat_83, -feat_15_feat_87, -feat_15_feat_88, -feat_15_feat_91, -feat_15_feat_93, -feat_16e2, -feat_15_feat_16e2, -feat_16e3, -feat_15e2_feat_21, -feat_15_feat_21e2, -feat_15e3_feat_21, -feat_15e2_feat_21e2, -feat_15_feat_21e3, -feat_54e2, -feat_15_feat_54e2, -feat_54e3, -feat_15e2_feat_62, -feat_15_feat_62e2, -feat_64e2, -feat_15e2_feat_64, -feat_15_feat_64e2, -feat_64e3, -feat_15e2_feat_66, -feat_15_feat_66e2, -feat_15e2_feat_70, -feat_15e2_feat_85, -feat_15e2_feat_88, -feat_15_feat_88e2, -feat_15e3_feat_88, -feat_15_feat_88e3)
  
  feat_9_14_25_combo <- data.frame(poly_9_data, poly_14_data, poly_25_data, 
                                   poly_40_data, poly_15_data) %>% 
    select(-contains(".")) %>% 
    select(-feat_1, -feat_2, -feat_4, -feat_6, -feat_7, -feat_10, -feat_13, -feat_21, -feat_23, -feat_28, -feat_30, -feat_31, -feat_35, -feat_37, -feat_38, -feat_45, -feat_46, -feat_49, -feat_52, -feat_58, -feat_59, -feat_61, -feat_64, -feat_65, -feat_73, -feat_74, -feat_82, -feat_83, -feat_85, -feat_88, -feat_89, -feat_92, -feat_93, -feat_9_feat_1, -feat_9_feat_19, -feat_9_feat_26, -feat_9_feat_29, -feat_9_feat_30, -feat_9_feat_38, -feat_9_feat_45, -feat_9_feat_64, -feat_9_feat_67, -feat_48e2, -feat_77e3, -feat_79e2, -feat_9e3_feat_79, -feat_88e4, -feat_14_feat_1, -feat_14_feat_2, -feat_14_feat_9, -feat_14_feat_24, -feat_14_feat_32, -feat_14_feat_38, -feat_14_feat_39, -feat_14_feat_44, -feat_14_feat_46, -feat_14_feat_47, -feat_14_feat_49, -feat_14_feat_53, -feat_14_feat_54, -feat_14_feat_59, -feat_14_feat_66, -feat_14_feat_68, -feat_14_feat_69, -feat_14_feat_72, -feat_14_feat_84, -feat_14_feat_87, -feat_14_feat_88, -feat_14_feat_90, -feat_14_feat_72e2, -feat_14_feat_72e3, -feat_14e2_feat_9, -feat_14_feat_9e2, -feat_9e3, -feat_48e3, -feat_59e3, -feat_25_feat_1, -feat_25_feat_8, -feat_25_feat_14, -feat_25_feat_19, -feat_25_feat_32, -feat_25_feat_36, -feat_25_feat_38, -feat_25_feat_39, -feat_25_feat_49, -feat_25_feat_54, -feat_25_feat_56, -feat_25_feat_62, -feat_25_feat_72, -feat_25_feat_83, -feat_25_feat_89, -feat_25_feat_92, -feat_25_feat_16e2, -feat_25e2_feat_40e2, -feat_42e2, -feat_42e3, -feat_25_feat_48e3, -feat_25_feat_54e2, -feat_25_feat_72e2, -feat_25_feat_72e3, -feat_88e2, -feat_88e3, -feat_25e2_feat_88e2) %>% select(-feat_22, -feat_29, -feat_77, -feat_87, -feat_9_feat_46, -feat_14_feat_22, -feat_14_feat_93, -feat_14e2_feat_15, -feat_14e2_feat_38, -feat_14_feat_38e2, -feat_14e2_feat_38e2, -feat_89e3, -feat_14e2_feat_40, -feat_14_feat_40e2, -feat_25_feat_3, -feat_25_feat_29, -feat_25_feat_31, -feat_25_feat_45, -feat_40e2_feat_15e2, -feat_40e3_feat_48, -feat_40e3_feat_79) %>%
    select(-feat_12, -feat_24, -feat_27, -feat_55, -feat_9_feat_21, -feat_9_feat_27, -feat_9_feat_58, -feat_9_feat_78, -feat_9_feat_79, -feat_9_feat_15e2, -feat_9e2_feat_79, -feat_14_feat_11, -feat_14_feat_15, -feat_14_feat_92, -feat_14_feat_15e2, -feat_14e2_feat_72e2, -feat_72e4, -feat_14e2_feat_33, -feat_33e3, -feat_14e2_feat_34, -feat_25_feat_65, -feat_16e2, -feat_25e2_feat_42, -feat_25e3_feat_42, -feat_40_feat_2, -feat_40_feat_4, -feat_40_feat_5, -feat_40_feat_6, -feat_40_feat_7, -feat_40_feat_9, -feat_40_feat_10, -feat_40_feat_12, -feat_40_feat_14, -feat_40_feat_19, -feat_40_feat_22, -feat_40_feat_23, -feat_40_feat_25, -feat_40_feat_28, -feat_40_feat_29, -feat_40_feat_31, -feat_40_feat_32, -feat_40_feat_35, -feat_40_feat_37, -feat_40_feat_38, -feat_40_feat_39, -feat_40_feat_41, -feat_40_feat_45, -feat_40_feat_47, -feat_40_feat_49, -feat_40_feat_51, -feat_40_feat_52, -feat_40_feat_53, -feat_40_feat_57, -feat_40_feat_58, -feat_40_feat_59, -feat_40_feat_60, -feat_40_feat_61, -feat_40_feat_63, -feat_40_feat_68, -feat_40_feat_71, -feat_40_feat_73, -feat_40_feat_76, -feat_40_feat_78, -feat_40_feat_80, -feat_40_feat_82, -feat_40_feat_83, -feat_40_feat_85, -feat_40_feat_86, -feat_40_feat_87, -feat_40_feat_88, -feat_40_feat_90, -feat_40_feat_91, -feat_40_feat_93, -feat_40_feat_15e2, -feat_40e3_feat_15, -feat_40_feat_15e3, -feat_40_feat_54e2, -feat_40e2_feat_67, -feat_15_feat_8, -feat_15_feat_11, -feat_15_feat_23, -feat_15_feat_25, -feat_15_feat_28, -feat_15_feat_30, -feat_15_feat_33, -feat_15_feat_34, -feat_15_feat_38, -feat_15_feat_40, -feat_15_feat_42, -feat_15_feat_84, -feat_15_feat_89, -feat_15_feat_70e2)
  
  return (feat_9_14_25_combo)
}

runAndValidateTraining <- function(data, current_class, datSetReduction = 1.0, description) {
  tmp_data <- data.frame(data, is_class = current_class) 
  set.seed(42)
  split.data <- createDataPartition(tmp_data$is_class, p = 0.6, list = FALSE)
  t.data <- tmp_data[split.data, ]
  tmp.test.data <- tmp_data[-split.data, ]
  set.seed(42)
  split.test <- createDataPartition(tmp.test.data$is_class, p = 0.5, list = FALSE)
  
  v.data <- tmp.test.data[split.test,]
  ts.data <- tmp.test.data[-split.test,]
  
  if (datSetReduction != 1.0) {
    set.seed(42)
    t.data <- t.data %>% sample_frac(datSetReduction, replace=FALSE)
  }
  
  control.config <- trainControl(method = "repeatedcv", repeats = 1,
                                 summaryFunction = twoClassSummary,
                                 classProbs = TRUE)
  
  a <- Sys.time()
  
  ada.grid <- expand.grid(.iter = c(50, 100),
                          .maxdepth = c(4, 8),
                          .nu = c(0.1, 1))
  
  rf.grid <- data.frame(.mtry = c(7,6,9))
  set.seed(42)
  logreg.tmp.train <- train(is_class ~ .,
                            data = t.data,
                            method = "rf",
                            metric = "ROC",
                            tuneGrid = rf.grid,
                            trControl = control.config)
  
  print (Sys.time() - a)
  logreg.tmp.pred_train <- predict(logreg.tmp.train)#, t.data)
  confusionMatrix(logreg.tmp.pred_train, t.data$is_class)$overall[1]
  
  logreg.tmp.pred <- predict(logreg.tmp.train, v.data)
  confusionMatrix(logreg.tmp.pred, v.data$is_class)$overall[1]
  
  print(summary(logreg.tmp.train))
  print(logreg.tmp.train)
  
  result <- data.frame(descr = description,train = confusionMatrix(logreg.tmp.pred_train, t.data$is_class)$overall[1], valid = confusionMatrix(logreg.tmp.pred, v.data$is_class)$overall[1])
  return (list(result=result, model = logreg.tmp.train, confMat = confusionMatrix(logreg.tmp.pred, v.data$is_class)))
}


createPolyFeatures <- function(df, degree = 2) {
  resultDf <- data.frame()
  tmpDf <- df
  colnames(tmpDf) <- c("x1", "x2")
  for (i in 1:degree) {
    for (j in 0:i) {
      feature_name <- createFeatureName(df, i, j)
      new_feature <- mutate(tmpDf, x_new = x1^(i-j) * x2^j) %>% select(x_new)
      colnames(new_feature) <- c(feature_name)
      if (i == 1 & j == 0) {
        resultDf <- new_feature
      } else {
        resultDf <- bind_cols(resultDf, new_feature)
      }
    }
  }
  return (resultDf)
}

createFeatureName <- function(df, i, j) {
  diff <- i-j
  baseName1 <- colnames(df)[1]
  baseName2 <- colnames(df)[2]
  
  name1 <- ifelse(diff == 0, "", 
                  ifelse(diff == 1, baseName1, 
                         paste(baseName1, "e", diff, sep="")))
  
  name2 <- ifelse(j == 0, "", 
                  ifelse(j == 1, baseName2, 
                         paste(baseName2, "e", j, sep="")))
  
  finalName <- ifelse(diff == 0, name2, 
                      ifelse(j == 0, name1, paste(name1,name2, sep="_")))
  return(finalName)
}


evaluateFeaturePolynoms <- function(primaryFeature, secondaryFeature, 
                                    trainingData, validationData, maxDegree = 6) {
  rs <- data.frame()
  names <- colnames(trainingData %>% select(primaryFeature,secondaryFeature))
  for (i in 1:maxDegree) {
    used_data <- 
      data.frame(createPolyFeatures(trainingData %>% 
                                      select(primaryFeature,secondaryFeature), i), 
                 is_class = trainingData$is_class)
    
    valid_data <- 
      data.frame(createPolyFeatures(validationData %>% 
                                      select(primaryFeature,secondaryFeature), i), 
                 is_class = validationData$is_class)
    
    control.config <- trainControl(method = "repeatedcv", repeats = 1,
                                   summaryFunction = twoClassSummary,
                                   classProbs = TRUE)
    
    training <- train(is_class ~ .,
                      data = used_data,
                      method = "glm",
                      metric = "ROC",
                      trControl = control.config)
    
    logreg.tmp.pred_train <- predict(training, used_data)
    a <- confusionMatrix(logreg.tmp.pred_train, used_data$is_class)$overall[1]
    
    logreg.tmp.pred <- predict(training, valid_data)
    b <- confusionMatrix(logreg.tmp.pred, valid_data$is_class)$overall[1]
    
    c <- data.frame(degree = i, train_acc = a, valid_acc = b)
    rs <- rbind(rs, c)
  }
  
  rs_vis <- rs %>% gather("type", "value", 2:3)
  
  test_plot <- ggplot(rs_vis, aes(x=degree, y=value, color=type)) +
    theme_classic() +
    geom_line() +
    ggtitle(paste(names, collapse="_"))
  return (list(results = rs, plot = test_plot))
}

enableParallel <- function() {
  require(doParallel)
  
  c1 <- makeCluster(2)
  registerDoParallel(c1)
}


createAllPolyFeatures <- function(data, primaryFeature, secondaryFeatures, degrees) {
  numberOfFeatures <- length(secondaryFeatures)
  if (numberOfFeatures != length(degrees)) {
    print("feature list does not fit to degree list")
    return (data.frame())
  }
  new_poly_df <- data.frame()
  for (i in 1:numberOfFeatures) {
    new_poly <- createPolyFeatures(data %>% 
                                     select(primaryFeature, secondaryFeatures[[i]]), degrees[[i]])
    if (i == 1) {
      new_poly_def <- data.frame(new_poly)
    } else {
      new_poly_def <- data.frame(new_poly_def, new_poly)
    }
  }
  return (new_poly_def %>% select(-contains(".")))
}


createComparisonPlot <- function(primaryFeature, secondaryFeature, data, current_class){
  if (primaryFeature == secondaryFeature) {
    return ()
  }
  plot_data <- data.frame(data %>% select(primaryFeature, secondaryFeature), is_class = current_class)
  orig_colnames <- colnames(plot_data)
  colnames(plot_data) <- c("x1", "x2", "is_class")
  
  pl <- ggplot(plot_data, aes(x=x1, y=x2, color=is_class)) +
    theme_classic() +
    geom_point(alpha=0.7) +
    xlab(orig_colnames[1]) +
    ylab(orig_colnames[2]) 
  
  folderName <- paste("plots/", orig_colnames[1], sep="")
  dir.create(folderName)
  filename <- paste(folderName, "/", primaryFeature, "_", secondaryFeature, ".png", sep="")
  
  ggsave(plot = pl, file=filename)
}