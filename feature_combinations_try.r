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
# engineered.data <- data.frame(feat_9_combinations, feat_14_combinations, 
#                               feat_25_combinations, feat_40_combinations, 
#                               feat_15_combinations, feat_72_combinations,
#                               feat_64_combinations, feat_88_combinations)

engineered.data <- addFeatureCombination(14, munged.data) %>% select(-feat_1, -feat_2, -feat_4, -feat_6, -feat_7, -feat_9, -feat_10, -feat_16, -feat_18, -feat_21, -feat_28, -feat_31, -feat_37, -feat_38, -feat_43, -feat_44, -feat_45, -feat_46, -feat_49, -feat_52, -feat_54, -feat_55, -feat_61, -feat_62, -feat_65, -feat_70, -feat_74, -feat_82, -feat_83, -feat_85, -feat_86, -feat_88, -feat_92, -feat_14_feat_4, -feat_14_feat_5, -feat_14_feat_6, -feat_14_feat_7, -feat_14_feat_10, -feat_14_feat_12, -feat_14_feat_18, -feat_14_feat_19, -feat_14_feat_20, -feat_14_feat_21, -feat_14_feat_22, -feat_14_feat_24, -feat_14_feat_27, -feat_14_feat_28, -feat_14_feat_31, -feat_14_feat_35, -feat_14_feat_41, -feat_14_feat_44, -feat_14_feat_45, -feat_14_feat_47, -feat_14_feat_49, -feat_14_feat_50, -feat_14_feat_52, -feat_14_feat_53, -feat_14_feat_55, -feat_14_feat_57, -feat_14_feat_58, -feat_14_feat_61, -feat_14_feat_63, -feat_14_feat_65, -feat_14_feat_67, -feat_14_feat_70, -feat_14_feat_71, -feat_14_feat_73, -feat_14_feat_75, -feat_14_feat_79, -feat_14_feat_81, -feat_14_feat_85, -feat_14_feat_86, -feat_14_feat_90)

engineered.data <- addFeatureCombination(9, munged.data)
#%>% 
   #select(feat_1:feat_93, feat_14_feat_1 , feat_14_feat_2 , feat_14_feat_3 , feat_14_feat_8 , feat_14_feat_9 , feat_14_feat_11, feat_14_feat_13, feat_14_feat_15, feat_14_feat_16, feat_14_feat_17, feat_14_feat_23, feat_14_feat_26, feat_14_feat_29, feat_14_feat_30, feat_14_feat_32, feat_14_feat_33, feat_14_feat_34, feat_14_feat_36, feat_14_feat_37, feat_14_feat_38, feat_14_feat_39, feat_14_feat_42, feat_14_feat_43, feat_14_feat_46, feat_14_feat_48, feat_14_feat_51, feat_14_feat_54, feat_14_feat_56, feat_14_feat_59, feat_14_feat_60, feat_14_feat_62, feat_14_feat_66, feat_14_feat_68, feat_14_feat_69, feat_14_feat_72, feat_14_feat_74, feat_14_feat_76, feat_14_feat_77, feat_14_feat_78, feat_14_feat_80, feat_14_feat_82, feat_14_feat_83, feat_14_feat_84, feat_14_feat_87, feat_14_feat_89, feat_14_feat_91, feat_14_feat_92, feat_14_feat_93, is_class2)
#feat_14_feat_1 , feat_14_feat_2 , feat_14_feat_3 , feat_14_feat_8 , feat_14_feat_9 , feat_14_feat_11, feat_14_feat_13, feat_14_feat_15, feat_14_feat_16, feat_14_feat_17, feat_14_feat_23, feat_14_feat_26, feat_14_feat_29, feat_14_feat_30, feat_14_feat_32, feat_14_feat_33, feat_14_feat_34, feat_14_feat_36, feat_14_feat_37, feat_14_feat_38, feat_14_feat_39, feat_14_feat_42, feat_14_feat_43, feat_14_feat_46, feat_14_feat_48, feat_14_feat_51, feat_14_feat_54, feat_14_feat_56, feat_14_feat_59, feat_14_feat_60, feat_14_feat_62, feat_14_feat_66, feat_14_feat_68, feat_14_feat_69, feat_14_feat_72, feat_14_feat_74, feat_14_feat_76, feat_14_feat_77, feat_14_feat_78, feat_14_feat_80, feat_14_feat_82, feat_14_feat_83, feat_14_feat_84, feat_14_feat_87, feat_14_feat_89, feat_14_feat_91, feat_14_feat_92, feat_14_feat_93
#feat_14_feat_1 , feat_14_feat_2 , feat_14_feat_3 , feat_14_feat_8, feat_14_feat_11, feat_14_feat_13, feat_14_feat_17, feat_14_feat_23, feat_14_feat_26, feat_14_feat_29, feat_14_feat_30, feat_14_feat_32, feat_14_feat_36, feat_14_feat_37, feat_14_feat_39, feat_14_feat_42, feat_14_feat_46, feat_14_feat_51, feat_14_feat_56, feat_14_feat_68, feat_14_feat_69, feat_14_feat_74, feat_14_feat_76, feat_14_feat_78, feat_14_feat_80, feat_14_feat_82, feat_14_feat_83, feat_14_feat_84, feat_14_feat_87, feat_14_feat_91, feat_14_feat_93,
poly_14_15 <- createPolyFeatures(munged.data %>% select(feat_14, feat_15), 3) %>% 
  select(-feat_14, -feat_15)
poly_14_72 <- createPolyFeatures(munged.data %>% select(feat_14, feat_72), 4) %>%
  select(-feat_14, -feat_72, -feat_14e2, -feat_14e3)

poly_14_9 <- createPolyFeatures(munged.data %>% select(feat_14, feat_9), 3) %>%
  select(-feat_14, -feat_9, -feat_14e2, -feat_14e3)

poly_14_16 <- createPolyFeatures(munged.data %>% select(feat_14, feat_16), 3) %>%
  select(-feat_14, -feat_16, -feat_14e2, -feat_14e3)

poly_14_43 <- createPolyFeatures(munged.data %>% select(feat_14, feat_43), 3) %>%
  select(-feat_14, -feat_43, -feat_14e2, -feat_14e3)

poly_14_33 <- createPolyFeatures(munged.data %>% select(feat_14, feat_33), 3) %>%
  select(-feat_14, -feat_33, -feat_14e2, -feat_14e3)

poly_14_34 <- createPolyFeatures(munged.data %>% select(feat_14, feat_34), 3) %>%
  select(-feat_14, -feat_34, -feat_14e2, -feat_14e3)

poly_14_38 <- createPolyFeatures(munged.data %>% select(feat_14, feat_38), 4) %>%
  select(-feat_14, -feat_38, -feat_14e2, -feat_14e3, -feat_14e4)

poly_14_48 <- createPolyFeatures(munged.data %>% select(feat_14, feat_48), 3) %>%
  select(-feat_14, -feat_48, -feat_14e2, -feat_14e3)

poly_14_54 <- createPolyFeatures(munged.data %>% select(feat_14, feat_54), 2) %>%
  select(-feat_14, -feat_54, -feat_14e2)

poly_14_59 <- createPolyFeatures(munged.data %>% select(feat_14, feat_59), 3) %>%
  select(-feat_14, -feat_59, -feat_14e2, -feat_14e3)

poly_14_60 <- createPolyFeatures(munged.data %>% select(feat_14, feat_60), 3) %>%
  select(-feat_14, -feat_60, -feat_14e2, -feat_14e3)

poly_14_62 <- createPolyFeatures(munged.data %>% select(feat_14, feat_62), 3) %>%
  select(-feat_14, -feat_62, -feat_14e2, -feat_14e3)

poly_14_66 <- createPolyFeatures(munged.data %>% select(feat_14, feat_66), 2) %>%
  select(-feat_14, -feat_66, -feat_14e2)

poly_14_77 <- createPolyFeatures(munged.data %>% select(feat_14, feat_77), 3) %>%
  select(-feat_14, -feat_77, -feat_14e2, -feat_14e3)

poly_14_89 <- createPolyFeatures(munged.data %>% select(feat_14, feat_89), 3) %>%
  select(-feat_14, -feat_89, -feat_14e2, -feat_14e3)

poly_14_92 <- createPolyFeatures(munged.data %>% select(feat_14, feat_92), 3) %>%
  select(-feat_14, -feat_92, -feat_14e2, -feat_14e3)

engineered.data <- data.frame(engineered.data, poly_14_15, poly_14_72, poly_14_9, 
                              poly_14_16, poly_14_43, poly_14_33, poly_14_34, 
                              poly_14_38, poly_14_48, poly_14_54, poly_14_59,
                              poly_14_60, poly_14_62, poly_14_66, poly_14_77,
                              poly_14_89, poly_14_92) %>% select(-feat_1, -feat_2, -feat_4, -feat_6, -feat_7, -feat_10, -feat_18, -feat_21, -feat_24, -feat_28, -feat_29, -feat_31, -feat_35, -feat_37, -feat_38, -feat_43, -feat_44, -feat_45, -feat_46, -feat_49, -feat_50, -feat_52, -feat_54, -feat_55, -feat_58, -feat_59, -feat_61, -feat_65, -feat_66, -feat_70, -feat_73, -feat_74, -feat_77, -feat_81, -feat_82, -feat_83, -feat_86, -feat_88, -feat_89, -feat_92, -feat_14_feat_3, -feat_14_feat_4, -feat_14_feat_5, -feat_14_feat_6, -feat_14_feat_7, -feat_14_feat_10, -feat_14_feat_12, -feat_14_feat_13, -feat_14_feat_16, -feat_14_feat_17, -feat_14_feat_18, -feat_14_feat_19, -feat_14_feat_20, -feat_14_feat_21, -feat_14_feat_23, -feat_14_feat_27, -feat_14_feat_28, -feat_14_feat_31, -feat_14_feat_33, -feat_14_feat_35, -feat_14_feat_37, -feat_14_feat_45, -feat_14_feat_48, -feat_14_feat_50, -feat_14_feat_52, -feat_14_feat_55, -feat_14_feat_57, -feat_14_feat_58, -feat_14_feat_61, -feat_14_feat_62, -feat_14_feat_65, -feat_14_feat_67, -feat_14_feat_70, -feat_14_feat_71, -feat_14_feat_73, -feat_14_feat_74, -feat_14_feat_75, -feat_14_feat_77, -feat_14_feat_78, -feat_14_feat_79, -feat_14_feat_81, -feat_14_feat_82, -feat_14_feat_85, -feat_14_feat_86, -feat_14_feat_89, -feat_14_feat_91, -feat_14_feat_15.1, -feat_14_feat_72.1, -feat_72e2, -feat_14e2_feat_72, -feat_72e3, -feat_14e4, -feat_14e3_feat_72, -feat_14_feat_9.1, -feat_14_feat_16.1, -feat_16e2, -feat_14_feat_16e2, -feat_16e3, -feat_14_feat_43.1, -feat_43e2, -feat_14e2_feat_43, -feat_14_feat_33.1, -feat_14_feat_33e2, -feat_14_feat_34.1, -feat_34e2, -feat_34e3, -feat_14_feat_38.1, -feat_38e2, -feat_38e3, -feat_14e3_feat_38, -feat_14_feat_38e3, -feat_38e4, -feat_14_feat_48.1, -feat_14_feat_48e2, -feat_14_feat_54.1, -feat_14_feat_59.1, -feat_14_feat_60.1, -feat_60e2, -feat_14e2_feat_60, -feat_14_feat_60e2, -feat_60e3, -feat_14_feat_62.1, -feat_14_feat_62e2, -feat_14_feat_66.1, -feat_14_feat_77.1, -feat_14e2_feat_77, -feat_14_feat_77e2, -feat_14_feat_89.1, -feat_14_feat_89e2, -feat_14_feat_92.1, -feat_92e2, -feat_14e2_feat_92, -feat_14_feat_92e2, -feat_92e3)

relevant_features <- list(6, 10, 14, 15, 20, 48, 50, 54, 56, 62, 64, 66, 70, 77, 79, 88)
degree_list <- list(3,3,3,3,2,2,3,3,4,4,4,2,3,3,4,4)

poly_9_feat <- createAllPolyFeatures(9, relevant_features, degree_list)

feat_9_specific <- data.frame(addFeatureCombination(9, munged.data),poly_9_feat) %>% select(-contains(".")) %>% select(-feat_1, -feat_2, -feat_3, -feat_4, -feat_6, -feat_10, -feat_18, -feat_21, -feat_22, -feat_28, -feat_31, -feat_35, -feat_37, -feat_43, -feat_44, -feat_45, -feat_49, -feat_50, -feat_52, -feat_54, -feat_65, -feat_66, -feat_73, -feat_77, -feat_82, -feat_83, -feat_89, -feat_93, -feat_9_feat_2, -feat_9_feat_3, -feat_9_feat_4, -feat_9_feat_5, -feat_9_feat_6, -feat_9_feat_8, -feat_9_feat_10, -feat_9_feat_12, -feat_9_feat_13, -feat_9_feat_15, -feat_9_feat_16, -feat_9_feat_18, -feat_9_feat_20, -feat_9_feat_22, -feat_9_feat_23, -feat_9_feat_25, -feat_9_feat_28, -feat_9_feat_31, -feat_9_feat_33, -feat_9_feat_35, -feat_9_feat_37, -feat_9_feat_40, -feat_9_feat_41, -feat_9_feat_42, -feat_9_feat_44, -feat_9_feat_47, -feat_9_feat_49, -feat_9_feat_50, -feat_9_feat_51, -feat_9_feat_52, -feat_9_feat_56, -feat_9_feat_59, -feat_9_feat_61, -feat_9_feat_62, -feat_9_feat_65, -feat_9_feat_66, -feat_9_feat_69, -feat_9_feat_70, -feat_9_feat_71, -feat_9_feat_73, -feat_9_feat_77, -feat_9_feat_80, -feat_9_feat_81, -feat_9_feat_83, -feat_9_feat_84, -feat_9_feat_85, -feat_9_feat_86, -feat_9_feat_87, -feat_9_feat_90, -feat_9_feat_91, -feat_9_feat_92, -feat_9_feat_93, -feat_6e2, -feat_9e3, -feat_9e2_feat_6, -feat_9_feat_6e2, -feat_6e3, -feat_10e2, -feat_9e2_feat_10, -feat_9_feat_10e2, -feat_10e3, -feat_9e2_feat_15, -feat_20e2, -feat_50e2, -feat_9_feat_50e2, -feat_50e3, -feat_56e2, -feat_9e2_feat_56, -feat_9_feat_56e2, -feat_56e3, -feat_9e4, -feat_9e3_feat_56, -feat_9e2_feat_56e2, -feat_9_feat_56e3, -feat_56e4, -feat_9e2_feat_62, -feat_9_feat_62e2, -feat_9e3_feat_62, -feat_9e2_feat_62e2, -feat_9_feat_62e3, -feat_62e4, -feat_9e2_feat_64, -feat_9e3_feat_64, -feat_64e4, -feat_9_feat_70e2, -feat_9e2_feat_77, -feat_9_feat_77e2, -feat_79e3, -feat_79e4, -feat_88e2, -feat_88e3, -feat_9_feat_88e3)
feat_14_specific <- data.frame(addFeatureCombination(14, munged.data), poly_14_15, poly_14_72, poly_14_9, 
                               poly_14_16, poly_14_43, poly_14_33, poly_14_34, 
                               poly_14_38, poly_14_48, poly_14_54, poly_14_59,
                               poly_14_60, poly_14_62, poly_14_66, poly_14_77,
                               poly_14_89, poly_14_92) %>% select(-feat_1, -feat_2, -feat_4, -feat_6, -feat_7, -feat_10, -feat_18, -feat_21, -feat_24, -feat_28, -feat_29, -feat_31, -feat_35, -feat_37, -feat_38, -feat_43, -feat_44, -feat_45, -feat_46, -feat_49, -feat_50, -feat_52, -feat_54, -feat_55, -feat_58, -feat_59, -feat_61, -feat_65, -feat_66, -feat_70, -feat_73, -feat_74, -feat_77, -feat_81, -feat_82, -feat_83, -feat_86, -feat_88, -feat_89, -feat_92, -feat_14_feat_3, -feat_14_feat_4, -feat_14_feat_5, -feat_14_feat_6, -feat_14_feat_7, -feat_14_feat_10, -feat_14_feat_12, -feat_14_feat_13, -feat_14_feat_16, -feat_14_feat_17, -feat_14_feat_18, -feat_14_feat_19, -feat_14_feat_20, -feat_14_feat_21, -feat_14_feat_23, -feat_14_feat_27, -feat_14_feat_28, -feat_14_feat_31, -feat_14_feat_33, -feat_14_feat_35, -feat_14_feat_37, -feat_14_feat_45, -feat_14_feat_48, -feat_14_feat_50, -feat_14_feat_52, -feat_14_feat_55, -feat_14_feat_57, -feat_14_feat_58, -feat_14_feat_61, -feat_14_feat_62, -feat_14_feat_65, -feat_14_feat_67, -feat_14_feat_70, -feat_14_feat_71, -feat_14_feat_73, -feat_14_feat_74, -feat_14_feat_75, -feat_14_feat_77, -feat_14_feat_78, -feat_14_feat_79, -feat_14_feat_81, -feat_14_feat_82, -feat_14_feat_85, -feat_14_feat_86, -feat_14_feat_89, -feat_14_feat_91, -feat_14_feat_15.1, -feat_14_feat_72.1, -feat_72e2, -feat_14e2_feat_72, -feat_72e3, -feat_14e4, -feat_14e3_feat_72, -feat_14_feat_9.1, -feat_14_feat_16.1, -feat_16e2, -feat_14_feat_16e2, -feat_16e3, -feat_14_feat_43.1, -feat_43e2, -feat_14e2_feat_43, -feat_14_feat_33.1, -feat_14_feat_33e2, -feat_14_feat_34.1, -feat_34e2, -feat_34e3, -feat_14_feat_38.1, -feat_38e2, -feat_38e3, -feat_14e3_feat_38, -feat_14_feat_38e3, -feat_38e4, -feat_14_feat_48.1, -feat_14_feat_48e2, -feat_14_feat_54.1, -feat_14_feat_59.1, -feat_14_feat_60.1, -feat_60e2, -feat_14e2_feat_60, -feat_14_feat_60e2, -feat_60e3, -feat_14_feat_62.1, -feat_14_feat_62e2, -feat_14_feat_66.1, -feat_14_feat_77.1, -feat_14e2_feat_77, -feat_14_feat_77e2, -feat_14_feat_89.1, -feat_14_feat_89e2, -feat_14_feat_92.1, -feat_92e2, -feat_14e2_feat_92, -feat_14_feat_92e2, -feat_92e3)

feat_9_14_combo <- data.frame(feat_9_specific, feat_14_specific) %>% select(-contains(".")) %>% select(-feat_29, -feat_38, -feat_59, -feat_61, -feat_64, -feat_74, -feat_86, -feat_92, -feat_9_feat_1, -feat_9_feat_29, -feat_9_feat_30, -feat_9_feat_38, -feat_9_feat_45, -feat_9_feat_67, -feat_9_feat_82, -feat_9_feat_89, -feat_54e3, -feat_79e2, -feat_88e4, -feat_14_feat_9, -feat_14_feat_24, -feat_14_feat_38, -feat_14_feat_39, -feat_14_feat_46, -feat_14_feat_47, -feat_14_feat_53, -feat_14_feat_54, -feat_14_feat_59, -feat_14_feat_68, -feat_14_feat_72, -feat_14_feat_87, -feat_14_feat_88, -feat_14_feat_90, -feat_14_feat_72e2, -feat_14e2_feat_72e2, -feat_14_feat_72e3, -feat_14e2_feat_9, -feat_14_feat_9e2, -feat_43e3, -feat_14e2_feat_33, -feat_59e3)

control_group <- data.frame(addFeatureCombination(9, munged.data), addFeatureCombination(14, munged.data)) %>% select(-contains(".")) %>% select(-feat_2, -feat_3, -feat_4, -feat_6, -feat_7, -feat_10, -feat_16, -feat_18, -feat_21, -feat_28, -feat_31, -feat_37, -feat_38, -feat_44, -feat_45, -feat_49, -feat_50, -feat_52, -feat_61, -feat_62, -feat_65, -feat_70, -feat_73, -feat_74, -feat_82, -feat_83, -feat_85, -feat_92, -feat_9_feat_2, -feat_9_feat_3, -feat_9_feat_4, -feat_9_feat_5, -feat_9_feat_6, -feat_9_feat_7, -feat_9_feat_8, -feat_9_feat_10, -feat_9_feat_12, -feat_9_feat_15, -feat_9_feat_16, -feat_9_feat_19, -feat_9_feat_22, -feat_9_feat_23, -feat_9_feat_28, -feat_9_feat_29, -feat_9_feat_30, -feat_9_feat_33, -feat_9_feat_35, -feat_9_feat_37, -feat_9_feat_38, -feat_9_feat_40, -feat_9_feat_41, -feat_9_feat_42, -feat_9_feat_44, -feat_9_feat_45, -feat_9_feat_47, -feat_9_feat_49, -feat_9_feat_51, -feat_9_feat_52, -feat_9_feat_54, -feat_9_feat_56, -feat_9_feat_57, -feat_9_feat_61, -feat_9_feat_62, -feat_9_feat_65, -feat_9_feat_66, -feat_9_feat_67, -feat_9_feat_69, -feat_9_feat_70, -feat_9_feat_71, -feat_9_feat_73, -feat_9_feat_77, -feat_9_feat_80, -feat_9_feat_81, -feat_9_feat_82, -feat_9_feat_83, -feat_9_feat_84, -feat_9_feat_85, -feat_9_feat_86, -feat_9_feat_87, -feat_9_feat_90, -feat_9_feat_91, -feat_9_feat_92, -feat_14_feat_4, -feat_14_feat_5, -feat_14_feat_6, -feat_14_feat_7, -feat_14_feat_9, -feat_14_feat_10, -feat_14_feat_12, -feat_14_feat_18, -feat_14_feat_19, -feat_14_feat_20, -feat_14_feat_21, -feat_14_feat_22, -feat_14_feat_23, -feat_14_feat_24, -feat_14_feat_27, -feat_14_feat_28, -feat_14_feat_31, -feat_14_feat_33, -feat_14_feat_35, -feat_14_feat_39, -feat_14_feat_44, -feat_14_feat_45, -feat_14_feat_46, -feat_14_feat_47, -feat_14_feat_48, -feat_14_feat_50, -feat_14_feat_52, -feat_14_feat_53, -feat_14_feat_54, -feat_14_feat_55, -feat_14_feat_57, -feat_14_feat_58, -feat_14_feat_61, -feat_14_feat_62, -feat_14_feat_67, -feat_14_feat_68, -feat_14_feat_70, -feat_14_feat_71, -feat_14_feat_72, -feat_14_feat_73, -feat_14_feat_74, -feat_14_feat_75, -feat_14_feat_79, -feat_14_feat_80, -feat_14_feat_85, -feat_14_feat_86, -feat_14_feat_87, -feat_14_feat_90)

relevant_features <- list(6, 10, 14, 15, 20, 48, 50, 54, 56, 62, 64, 66, 70, 77, 79, 88)
degree_list <- list(3,3,3,3,2,2,3,3,4,4,4,2,3,3,4,4)

poly_9_feat <- createAllPolyFeatures(9, relevant_features, degree_list)

engineered.data <- data.frame(engineered.data,poly_9_feat) %>% select(-contains(".")) %>% select(-feat_1, -feat_2, -feat_3, -feat_4, -feat_6, -feat_10, -feat_18, -feat_21, -feat_22, -feat_28, -feat_31, -feat_35, -feat_37, -feat_43, -feat_44, -feat_45, -feat_49, -feat_50, -feat_52, -feat_54, -feat_65, -feat_66, -feat_73, -feat_77, -feat_82, -feat_83, -feat_89, -feat_93, -feat_9_feat_2, -feat_9_feat_3, -feat_9_feat_4, -feat_9_feat_5, -feat_9_feat_6, -feat_9_feat_8, -feat_9_feat_10, -feat_9_feat_12, -feat_9_feat_13, -feat_9_feat_15, -feat_9_feat_16, -feat_9_feat_18, -feat_9_feat_20, -feat_9_feat_22, -feat_9_feat_23, -feat_9_feat_25, -feat_9_feat_28, -feat_9_feat_31, -feat_9_feat_33, -feat_9_feat_35, -feat_9_feat_37, -feat_9_feat_40, -feat_9_feat_41, -feat_9_feat_42, -feat_9_feat_44, -feat_9_feat_47, -feat_9_feat_49, -feat_9_feat_50, -feat_9_feat_51, -feat_9_feat_52, -feat_9_feat_56, -feat_9_feat_59, -feat_9_feat_61, -feat_9_feat_62, -feat_9_feat_65, -feat_9_feat_66, -feat_9_feat_69, -feat_9_feat_70, -feat_9_feat_71, -feat_9_feat_73, -feat_9_feat_77, -feat_9_feat_80, -feat_9_feat_81, -feat_9_feat_83, -feat_9_feat_84, -feat_9_feat_85, -feat_9_feat_86, -feat_9_feat_87, -feat_9_feat_90, -feat_9_feat_91, -feat_9_feat_92, -feat_9_feat_93, -feat_6e2, -feat_9e3, -feat_9e2_feat_6, -feat_9_feat_6e2, -feat_6e3, -feat_10e2, -feat_9e2_feat_10, -feat_9_feat_10e2, -feat_10e3, -feat_9e2_feat_15, -feat_20e2, -feat_50e2, -feat_9_feat_50e2, -feat_50e3, -feat_56e2, -feat_9e2_feat_56, -feat_9_feat_56e2, -feat_56e3, -feat_9e4, -feat_9e3_feat_56, -feat_9e2_feat_56e2, -feat_9_feat_56e3, -feat_56e4, -feat_9e2_feat_62, -feat_9_feat_62e2, -feat_9e3_feat_62, -feat_9e2_feat_62e2, -feat_9_feat_62e3, -feat_62e4, -feat_9e2_feat_64, -feat_9e3_feat_64, -feat_64e4, -feat_9_feat_70e2, -feat_9e2_feat_77, -feat_9_feat_77e2, -feat_79e3, -feat_79e4, -feat_88e2, -feat_88e3, -feat_9_feat_88e3)

#%>% select(-feat_4, -feat_6, -feat_7, -feat_10, -feat_18, -feat_21, -feat_22, -feat_28, -feat_31, -feat_35, -feat_37, -feat_38, -feat_43, -feat_44, -feat_45, -feat_49, -feat_52, -feat_54, -feat_59, -feat_65, -feat_66, -feat_73, -feat_74, -feat_77, -feat_81, -feat_82, -feat_83, -feat_86, -feat_89, -feat_92, -feat_93, -feat_72e2, -feat_14e2_feat_72, -feat_14e3_feat_72, -feat_14_feat_16, -feat_14_feat_16e2, -feat_16e3, -feat_43e2, -feat_14_feat_33, -feat_14_feat_33e2, -feat_34e2, -feat_34e3, -feat_38e2, -feat_38e3, -feat_14e3_feat_38, -feat_14_feat_38e3, -feat_38e4, -feat_14_feat_48e2, -feat_14_feat_59, -feat_59e3, -feat_60e2, -feat_14e2_feat_60, -feat_14_feat_60e2, -feat_60e3, -feat_14_feat_62, -feat_14_feat_62e2, -feat_14_feat_66, -feat_14_feat_77e2, -feat_14_feat_89, -feat_14_feat_89e2, -feat_92e2, -feat_14_feat_92e2, -feat_92e3)

#engineered.data <- normalize(munged.data)

new_result <- runAndValidateTraining(control_group, 
                                     "9and14 basic non sig removed")

current_results <- rbind(current_results, new_result)







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
current_results <- rbind(current_results, data.frame(descr = "Removed basic combos",train = confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)$overall[1], valid = confusionMatrix(logreg.tmp.pred, v.data$is_class2)$overall[1]))

logreg.tmp.test <- predict(logreg.tmp.train, ts.data)
confusionMatrix(logreg.tmp.test, ts.data$is_class2)$overall[1]