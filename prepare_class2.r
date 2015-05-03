setwd("C:/dev/repositories/R/kaggle_otto")
source("init_ws.r")

prepareDataClass2 <- function(data) {
  base_data_feat_14 <- addFeatureCombination(14, data)
  
  relevant_features_14 <- list(15,72,9,16,43,33,34,38,48,59,60,62,66,77,89,92,40)
  degree_list_14 <- list(3,4,3,3,3,3,3,4,3,3,3,3,2,3,3,3,3)
  poly_14_feat <- createAllPolyFeatures(14, relevant_features_14, degree_list_14)
  
  poly_14_data <- data.frame(base_data_feat_14, poly_14_feat) %>% select(-contains(".")) %>% select(-feat_14_feat_3, -feat_14_feat_4, -feat_14_feat_5, -feat_14_feat_6, -feat_14_feat_7, -feat_14_feat_10, -feat_14_feat_12, -feat_14_feat_13, -feat_14_feat_16, -feat_14_feat_17, -feat_14_feat_18, -feat_14_feat_19, -feat_14_feat_20, -feat_14_feat_21, -feat_14_feat_23, -feat_14_feat_27, -feat_14_feat_28, -feat_14_feat_31, -feat_14_feat_33, -feat_14_feat_35, -feat_14_feat_37, -feat_14_feat_45, -feat_14_feat_48, -feat_14_feat_50, -feat_14_feat_52, -feat_14_feat_55, -feat_14_feat_57, -feat_14_feat_58, -feat_14_feat_61, -feat_14_feat_62, -feat_14_feat_65, -feat_14_feat_67, -feat_14_feat_70, -feat_14_feat_71, -feat_14_feat_73, -feat_14_feat_74, -feat_14_feat_75, -feat_14_feat_77, -feat_14_feat_78, -feat_14_feat_79, -feat_14_feat_81, -feat_14_feat_82, -feat_14_feat_85, -feat_14_feat_86, -feat_14_feat_89, -feat_14_feat_91, -feat_72e2, -feat_14e2_feat_72, -feat_72e3, -feat_14e4, -feat_14e3_feat_72, -feat_16e2, -feat_14_feat_16e2, -feat_16e3, -feat_43e2, -feat_14e2_feat_43, -feat_14_feat_33e2, -feat_34e2, -feat_34e3, -feat_38e2, -feat_38e3, -feat_14e3_feat_38, -feat_14_feat_38e3, -feat_38e4, -feat_14_feat_48e2, -feat_60e2, -feat_14e2_feat_60, -feat_14_feat_60e2, -feat_60e3, -feat_14_feat_62e2, -feat_14e2_feat_77, -feat_14_feat_77e2, -feat_14_feat_89e2, -feat_92e2, -feat_14e2_feat_92, -feat_14_feat_92e2, -feat_92e3)
  
  base_data_feat_9 <- addFeatureCombination(9, data)
  
  relevant_features_9 <- list(6, 10, 14, 15, 20, 48, 50, 54, 56, 62, 64, 66, 70, 77, 79, 88)
  degree_list_9 <- list(3,3,3,3,2,2,3,3,4,4,4,2,3,3,4,4)
  
  poly_9_feat <- createAllPolyFeatures(9, relevant_features_9, degree_list_9)
  
  poly_9_data <- data.frame(base_data_feat_9, poly_9_feat) %>% select(-contains(".")) %>% select(-feat_9_feat_2, -feat_9_feat_3, -feat_9_feat_4, -feat_9_feat_5, -feat_9_feat_6, -feat_9_feat_8, -feat_9_feat_10, -feat_9_feat_12, -feat_9_feat_13, -feat_9_feat_15, -feat_9_feat_16, -feat_9_feat_18, -feat_9_feat_20, -feat_9_feat_22, -feat_9_feat_23, -feat_9_feat_25, -feat_9_feat_28, -feat_9_feat_31, -feat_9_feat_33, -feat_9_feat_35, -feat_9_feat_37, -feat_9_feat_40, -feat_9_feat_41, -feat_9_feat_42, -feat_9_feat_44, -feat_9_feat_47, -feat_9_feat_49, -feat_9_feat_50, -feat_9_feat_51, -feat_9_feat_52, -feat_9_feat_56, -feat_9_feat_59, -feat_9_feat_61, -feat_9_feat_62, -feat_9_feat_65, -feat_9_feat_66, -feat_9_feat_69, -feat_9_feat_70, -feat_9_feat_71, -feat_9_feat_73, -feat_9_feat_77, -feat_9_feat_80, -feat_9_feat_81, -feat_9_feat_83, -feat_9_feat_84, -feat_9_feat_85, -feat_9_feat_86, -feat_9_feat_87, -feat_9_feat_90, -feat_9_feat_91, -feat_9_feat_92, -feat_9_feat_93, -feat_6e2, -feat_9e3, -feat_9e2_feat_6, -feat_9_feat_6e2, -feat_6e3, -feat_10e2, -feat_9e2_feat_10, -feat_9_feat_10e2, -feat_10e3, -feat_9e2_feat_15, -feat_20e2, -feat_50e2, -feat_9_feat_50e2, -feat_50e3, -feat_56e2, -feat_9e2_feat_56, -feat_9_feat_56e2, -feat_56e3, -feat_9e4, -feat_9e3_feat_56, -feat_9e2_feat_56e2, -feat_9_feat_56e3, -feat_56e4, -feat_9e2_feat_62, -feat_9_feat_62e2, -feat_9e3_feat_62, -feat_9e2_feat_62e2, -feat_9_feat_62e3, -feat_62e4, -feat_9e2_feat_64, -feat_9e3_feat_64, -feat_64e4, -feat_9_feat_70e2, -feat_9e2_feat_77, -feat_9_feat_77e2, -feat_79e3, -feat_79e4, -feat_88e2, -feat_88e3, -feat_9_feat_88e3)
  
  base_data_feat_25 <- addFeatureCombination(25, data)
  
  relevant_features_25 <- list(14, 15, 16, 40, 42, 44, 48, 54, 66, 67, 72, 88)
  degree_list_25 <- list(3,4,3,4,4,3,4,3,3,5,4,4)
  
  poly_25_feat <- createAllPolyFeatures(25, relevant_features_25, degree_list_25)
  
  poly_25_data <- data.frame(base_data_feat_25, poly_25_feat) %>% select(-contains(".")) %>% select(-feat_25_feat_2, -feat_25_feat_4, -feat_25_feat_6, -feat_25_feat_7, -feat_25_feat_9, -feat_25_feat_10, -feat_25_feat_12, -feat_25_feat_16, -feat_25_feat_17, -feat_25_feat_21, -feat_25_feat_22, -feat_25_feat_26, -feat_25_feat_27, -feat_25_feat_28, -feat_25_feat_33, -feat_25_feat_35, -feat_25_feat_37, -feat_25_feat_42, -feat_25_feat_43, -feat_25_feat_46, -feat_25_feat_47, -feat_25_feat_48, -feat_25_feat_50, -feat_25_feat_51, -feat_25_feat_52, -feat_25_feat_53, -feat_25_feat_55, -feat_25_feat_57, -feat_25_feat_58, -feat_25_feat_59, -feat_25_feat_61, -feat_25_feat_63, -feat_25_feat_64, -feat_25_feat_66, -feat_25_feat_68, -feat_25_feat_70, -feat_25_feat_71, -feat_25_feat_73, -feat_25_feat_75, -feat_25_feat_76, -feat_25_feat_79, -feat_25_feat_80, -feat_25_feat_81, -feat_25_feat_82, -feat_25_feat_87, -feat_25_feat_90, -feat_25_feat_91, -feat_25_feat_93, -feat_25e3, -feat_25e2_feat_14, -feat_25e4, -feat_25e2_feat_16, -feat_16e3, -feat_25_feat_40e2, -feat_25e3_feat_40, -feat_25_feat_40e3, -feat_25e2_feat_42e2, -feat_25_feat_42e3, -feat_42e4, -feat_44e2, -feat_25_feat_44e2, -feat_44e3, -feat_48e2, -feat_25e2_feat_48, -feat_48e3, -feat_48e4, -feat_25e2_feat_54, -feat_25e2_feat_66, -feat_25_feat_66e2, -feat_25e2_feat_67, -feat_25_feat_67e3, -feat_25e2_feat_67e3, -feat_25_feat_67e4, -feat_72e2, -feat_25e2_feat_72, -feat_72e3, -feat_25e2_feat_72e2)
  
  base_data_feat_40 <- addFeatureCombination(40, data) #munged.data %>% select(1:93, is_class2)
  
  relevant_features_40 <- list(15,18,44,48,54,62,66,67,70,79,89)
  degree_list_40 <- list(4,3,4,4,3,3,4,3,4,4,3)
  
  poly_40_feat <- createAllPolyFeatures(40, relevant_features_40, degree_list_40)
  
  poly_40_data <- data.frame(base_data_feat_40, poly_40_feat) %>% 
    select(-contains(".")) %>% select(-feat_2, -feat_4, -feat_7, -feat_10, -feat_21, -feat_28, -feat_31, -feat_35, -feat_37, -feat_38, -feat_44, -feat_45, -feat_49, -feat_52, -feat_55, -feat_65, -feat_70, -feat_74, -feat_76, -feat_82, -feat_83, -feat_86, -feat_87, -feat_89, -feat_93, -feat_40_feat_18, -feat_18e2, -feat_40e2_feat_18, -feat_18e3, -feat_44e2, -feat_40_feat_44e2, -feat_44e3, -feat_40e3_feat_44, -feat_40e2_feat_44e2, -feat_40_feat_44e3, -feat_44e4, -feat_48e3, -feat_48e4, -feat_40_feat_62, -feat_40e2_feat_62, -feat_40_feat_62e2, -feat_40_feat_66, -feat_40e2_feat_66, -feat_40_feat_66e2, -feat_66e3, -feat_40e2_feat_66e2, -feat_66e4, -feat_40_feat_70, -feat_70e2, -feat_40e2_feat_70, -feat_40_feat_70e2, -feat_70e3, -feat_40e2_feat_70e2, -feat_40_feat_70e3, -feat_70e4, -feat_79e2, -feat_79e3, -feat_40_feat_79e3, -feat_79e4, -feat_40_feat_89, -feat_89e2, -feat_40_feat_89e2, -feat_89e3)
  
  base_data_feat_15 <- addFeatureCombination(15, data)# %>% select(1:93, is_class2)
  relevant_features_15 <- list(16,21,48,54,62,64,66,67,70,85,88)
  degree_list_15 <- list(3,4,3,3,3,3,3,4,3,3,4)
  poly_15_feat <- createAllPolyFeatures(15, relevant_features_15, degree_list_15)
  poly_15_data <- data.frame(base_data_feat_15, poly_15_feat) %>% select(-contains(".")) %>%
    select(-feat_1, -feat_3, -feat_4, -feat_5, -feat_7, -feat_10, -feat_12, -feat_18, -feat_24, -feat_37, -feat_43, -feat_44, -feat_45, -feat_49, -feat_52, -feat_54, -feat_74, -feat_76, -feat_81, -feat_82, -feat_83, -feat_93, -feat_15_feat_2, -feat_15_feat_3, -feat_15_feat_4, -feat_15_feat_5, -feat_15_feat_6, -feat_15_feat_7, -feat_15_feat_12, -feat_15_feat_13, -feat_15_feat_14, -feat_15_feat_18, -feat_15_feat_19, -feat_15_feat_20, -feat_15_feat_21, -feat_15_feat_22, -feat_15_feat_26, -feat_15_feat_27, -feat_15_feat_29, -feat_15_feat_32, -feat_15_feat_35, -feat_15_feat_36, -feat_15_feat_41, -feat_15_feat_44, -feat_15_feat_46, -feat_15_feat_47, -feat_15_feat_49, -feat_15_feat_50, -feat_15_feat_51, -feat_15_feat_52, -feat_15_feat_53, -feat_15_feat_55, -feat_15_feat_56, -feat_15_feat_58, -feat_15_feat_60, -feat_15_feat_61, -feat_15_feat_62, -feat_15_feat_64, -feat_15_feat_65, -feat_15_feat_66, -feat_15_feat_68, -feat_15_feat_69, -feat_15_feat_70, -feat_15_feat_74, -feat_15_feat_75, -feat_15_feat_76, -feat_15_feat_77, -feat_15_feat_78, -feat_15_feat_81, -feat_15_feat_82, -feat_15_feat_83, -feat_15_feat_87, -feat_15_feat_88, -feat_15_feat_91, -feat_15_feat_93, -feat_16e2, -feat_15_feat_16e2, -feat_16e3, -feat_15e2_feat_21, -feat_15_feat_21e2, -feat_15e3_feat_21, -feat_15e2_feat_21e2, -feat_15_feat_21e3, -feat_54e2, -feat_15_feat_54e2, -feat_54e3, -feat_15e2_feat_62, -feat_15_feat_62e2, -feat_64e2, -feat_15e2_feat_64, -feat_15_feat_64e2, -feat_64e3, -feat_15e2_feat_66, -feat_15_feat_66e2, -feat_15e2_feat_70, -feat_15e2_feat_85, -feat_15e2_feat_88, -feat_15_feat_88e2, -feat_15e3_feat_88, -feat_15_feat_88e3)
  
  feat_9_14_25_combo <- data.frame(poly_9_data, poly_14_data, poly_25_data, 
                                   poly_40_data, poly_15_data) %>% 
    select(-contains(".")) %>% 
    select(-feat_1, -feat_2, -feat_4, -feat_6, -feat_7, -feat_10, -feat_13, -feat_21, -feat_23, -feat_28, -feat_30, -feat_31, -feat_35, -feat_37, -feat_38, -feat_45, -feat_46, -feat_49, -feat_52, -feat_58, -feat_59, -feat_61, -feat_64, -feat_65, -feat_73, -feat_74, -feat_82, -feat_83, -feat_85, -feat_88, -feat_89, -feat_92, -feat_93, -feat_9_feat_1, -feat_9_feat_19, -feat_9_feat_26, -feat_9_feat_29, -feat_9_feat_30, -feat_9_feat_38, -feat_9_feat_45, -feat_9_feat_64, -feat_9_feat_67, -feat_48e2, -feat_77e3, -feat_79e2, -feat_9e3_feat_79, -feat_88e4, -feat_14_feat_1, -feat_14_feat_2, -feat_14_feat_9, -feat_14_feat_24, -feat_14_feat_32, -feat_14_feat_38, -feat_14_feat_39, -feat_14_feat_44, -feat_14_feat_46, -feat_14_feat_47, -feat_14_feat_49, -feat_14_feat_53, -feat_14_feat_54, -feat_14_feat_59, -feat_14_feat_66, -feat_14_feat_68, -feat_14_feat_69, -feat_14_feat_72, -feat_14_feat_84, -feat_14_feat_87, -feat_14_feat_88, -feat_14_feat_90, -feat_14_feat_72e2, -feat_14_feat_72e3, -feat_14e2_feat_9, -feat_14_feat_9e2, -feat_9e3, -feat_48e3, -feat_59e3, -feat_25_feat_1, -feat_25_feat_8, -feat_25_feat_14, -feat_25_feat_19, -feat_25_feat_32, -feat_25_feat_36, -feat_25_feat_38, -feat_25_feat_39, -feat_25_feat_49, -feat_25_feat_54, -feat_25_feat_56, -feat_25_feat_62, -feat_25_feat_72, -feat_25_feat_83, -feat_25_feat_89, -feat_25_feat_92, -feat_25_feat_16e2, -feat_25e2_feat_40e2, -feat_42e2, -feat_42e3, -feat_25_feat_48e3, -feat_25_feat_54e2, -feat_25_feat_72e2, -feat_25_feat_72e3, -feat_88e2, -feat_88e3, -feat_25e2_feat_88e2) %>% select(-feat_22, -feat_29, -feat_77, -feat_87, -feat_9_feat_46, -feat_14_feat_22, -feat_14_feat_93, -feat_14e2_feat_15, -feat_14e2_feat_38, -feat_14_feat_38e2, -feat_14e2_feat_38e2, -feat_89e3, -feat_14e2_feat_40, -feat_14_feat_40e2, -feat_25_feat_3, -feat_25_feat_29, -feat_25_feat_31, -feat_25_feat_45, -feat_40e2_feat_15e2, -feat_40e3_feat_48, -feat_40e3_feat_79) %>%
    select(-feat_12, -feat_24, -feat_27, -feat_55, -feat_9_feat_21, -feat_9_feat_27, -feat_9_feat_58, -feat_9_feat_78, -feat_9_feat_79, -feat_9_feat_15e2, -feat_9e2_feat_79, -feat_14_feat_11, -feat_14_feat_15, -feat_14_feat_92, -feat_14_feat_15e2, -feat_14e2_feat_72e2, -feat_72e4, -feat_14e2_feat_33, -feat_33e3, -feat_14e2_feat_34, -feat_25_feat_65, -feat_16e2, -feat_25e2_feat_42, -feat_25e3_feat_42, -feat_40_feat_2, -feat_40_feat_4, -feat_40_feat_5, -feat_40_feat_6, -feat_40_feat_7, -feat_40_feat_9, -feat_40_feat_10, -feat_40_feat_12, -feat_40_feat_14, -feat_40_feat_19, -feat_40_feat_22, -feat_40_feat_23, -feat_40_feat_25, -feat_40_feat_28, -feat_40_feat_29, -feat_40_feat_31, -feat_40_feat_32, -feat_40_feat_35, -feat_40_feat_37, -feat_40_feat_38, -feat_40_feat_39, -feat_40_feat_41, -feat_40_feat_45, -feat_40_feat_47, -feat_40_feat_49, -feat_40_feat_51, -feat_40_feat_52, -feat_40_feat_53, -feat_40_feat_57, -feat_40_feat_58, -feat_40_feat_59, -feat_40_feat_60, -feat_40_feat_61, -feat_40_feat_63, -feat_40_feat_68, -feat_40_feat_71, -feat_40_feat_73, -feat_40_feat_76, -feat_40_feat_78, -feat_40_feat_80, -feat_40_feat_82, -feat_40_feat_83, -feat_40_feat_85, -feat_40_feat_86, -feat_40_feat_87, -feat_40_feat_88, -feat_40_feat_90, -feat_40_feat_91, -feat_40_feat_93, -feat_40_feat_15e2, -feat_40e3_feat_15, -feat_40_feat_15e3, -feat_40_feat_54e2, -feat_40e2_feat_67, -feat_15_feat_8, -feat_15_feat_11, -feat_15_feat_23, -feat_15_feat_25, -feat_15_feat_28, -feat_15_feat_30, -feat_15_feat_33, -feat_15_feat_34, -feat_15_feat_38, -feat_15_feat_40, -feat_15_feat_42, -feat_15_feat_84, -feat_15_feat_89, -feat_15_feat_70e2)
  
  return (feat_9_14_25_combo)
}



dataForClass2 <- prepareDataClass2(munged.data)

control.config <- trainControl(method = "repeatedcv", repeats = 1,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

logreg.tmp.train <- train(is_class2 ~ .,
                          data = dataForClass2,
                          method = "glm",
                          metric = "ROC",
                          trControl = control.config)

logreg.tmp.pred_train <- predict(logreg.tmp.train, dataForClass2)
confusionMatrix(logreg.tmp.pred_train, munged.data$is_class2)
