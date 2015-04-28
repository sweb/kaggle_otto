ggplot(engineered.data, aes(x=feat_14, y=feat_93, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

#feat_14_feat_1 , feat_14_feat_2 , feat_14_feat_3 , feat_14_feat_8 , feat_14_feat_9 , 
#feat_14_feat_11, feat_14_feat_13, feat_14_feat_15, feat_14_feat_16, feat_14_feat_17, 
#feat_14_feat_23, feat_14_feat_26, feat_14_feat_29, feat_14_feat_30, feat_14_feat_32, 
#feat_14_feat_33, feat_14_feat_34, feat_14_feat_36, feat_14_feat_37, feat_14_feat_38, 
#feat_14_feat_39, feat_14_feat_42, feat_14_feat_43, feat_14_feat_46, feat_14_feat_48, 
#feat_14_feat_51, feat_14_feat_54, feat_14_feat_56, feat_14_feat_59, feat_14_feat_60, 
#feat_14_feat_62, feat_14_feat_66, feat_14_feat_68, feat_14_feat_69, feat_14_feat_72, 
#feat_14_feat_74, feat_14_feat_76, feat_14_feat_77, feat_14_feat_78, feat_14_feat_80, 
#feat_14_feat_82, feat_14_feat_83, feat_14_feat_84, feat_14_feat_87, feat_14_feat_89, 
#feat_14_feat_91, feat_14_feat_92, feat_14_feat_93

createPolyFeatures <- function(df, degree = 2) {
  resultDf <- data.frame()
  tmpDf <- df
  colnames(tmpDf) <- c("x1", "x2")
  for (i in 1:degree) {
    for (j in 0:i) {
      feature_name <- paste(colnames(df)[1], "e", i-j, "_", colnames(df)[2], "e", j, sep="")
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

logreg.tmp.train$finalModel$coefficients[c(0,14,9)+1]

inter <- -0.2783963 / -1.4172225
slope <- 1.2752838 / -1.4172225

ggplot(munged.data, aes(x=feat_14, y=feat_9, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7) +
  stat_function(fun=function(x)x+inter, geom="line")

evaluateFeaturePolynoms <- function(primaryFeature, secondaryFeature, maxDegree = 8) {
  rs <- data.frame()
  
  for (i in 1:maxDegree) {
    used_data <- 
      data.frame(createPolyFeatures(train.data %>% 
                                      select(primaryFeature,secondaryFeature), i), 
                 is_class2 = train.data$is_class2)
    
    valid_data <- 
      data.frame(createPolyFeatures(validation.data %>% 
                                      select(primaryFeature,secondaryFeature), i), 
                 is_class2 = validation.data$is_class2)
    
    training <- train(is_class2 ~ .,
                                   data = used_data,
                                   method = "glm",
                                   metric = "ROC",
                                   trControl = control.config)
    
    logreg.tmp.pred_train <- predict(training, used_data)
    a <- confusionMatrix(logreg.tmp.pred_train, used_data$is_class2)$overall[1]
    
    logreg.tmp.pred <- predict(training, valid_data)
    b <- confusionMatrix(logreg.tmp.pred, valid_data$is_class2)$overall[1]
    
    c <- data.frame(degree = i, train_acc = a, valid_acc = b)
    rs <- rbind(rs, c)
  }
  
  rs_vis <- rs %>% gather("type", "value", 2:3)
  
  test_plot <- ggplot(rs_vis, aes(x=degree, y=value, color=type)) +
    theme_classic() +
    geom_line()
  return (list(results = rs, plot = test_plot))
}

p <- evaluateFeaturePolynoms(14,9,5)
#14,9 => 3 => 0.7742135 0.7719780

ggplot(train.data, aes(x=feat_14, y=feat_15, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

p <- evaluateFeaturePolynoms(14,15,8)
# => 4 ggf. 3 => 0.7896197 0.7847447

ggplot(engineered.data, aes(x=feat_14, y=feat_16, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

p <- evaluateFeaturePolynoms(14,16,7)
# => 3 => 0.7753986 0.7692308

ggplot(train.data, aes(x=feat_14, y=feat_43, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

p <- evaluateFeaturePolynoms(14,43,7)
# => 3 => 0.7790886 0.7735133

ggplot(train.data, aes(x=feat_14, y=feat_33, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

p <- evaluateFeaturePolynoms(14,33,7)
3# 0.7758026 0.7721396

ggplot(engineered.data, aes(x=feat_14, y=feat_34, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_38, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_48, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_54, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_59, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_60, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_62, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_66, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_72, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_77, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_89, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)

ggplot(engineered.data, aes(x=feat_14, y=feat_92, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)