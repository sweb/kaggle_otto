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



logreg.tmp.train$finalModel$coefficients[c(0,14,9)+1]

inter <- -0.2783963 / -1.4172225
slope <- 1.2752838 / -1.4172225

ggplot(munged.data, aes(x=feat_14, y=feat_9, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7) +
  stat_function(fun=function(x)x+inter, geom="line")


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



results <- list()
length(results) <- 12
counter <- 1
for (i in list(34, 38, 48, 54, 59, 60, 62, 66, 72, 77, 89, 92)) {
  results[[counter]] <- evaluateFeaturePolynoms(14,i,7)
  counter <- counter + 1
}

ggplot(engineered.data, aes(x=feat_14, y=feat_34, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7754525 0.7704428

ggplot(engineered.data, aes(x=feat_14, y=feat_38, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#4 0.7820243 0.7750485

ggplot(engineered.data, aes(x=feat_14, y=feat_48, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7800582 0.7704428

ggplot(engineered.data, aes(x=feat_14, y=feat_54, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#2 0.7760450 0.7721396

ggplot(engineered.data, aes(x=feat_14, y=feat_59, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7771493 0.7714124

ggplot(engineered.data, aes(x=feat_14, y=feat_60, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7807854 0.7751293

ggplot(engineered.data, aes(x=feat_14, y=feat_62, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7763144 0.7718972

ggplot(engineered.data, aes(x=feat_14, y=feat_66, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#0.7814857 0.7760181 ggf 2

ggplot(engineered.data, aes(x=feat_14, y=feat_72, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#4 0.7916128 0.7846639

ggplot(engineered.data, aes(x=feat_14, y=feat_77, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7755333 0.7702004

ggplot(engineered.data, aes(x=feat_14, y=feat_89, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7782536 0.7729476

ggplot(engineered.data, aes(x=feat_14, y=feat_92, color=is_class2)) +
  theme_classic() +
  geom_point(alpha=0.7)
#3 0.7749677 0.7698772
