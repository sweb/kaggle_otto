summary(munged.data %>%
          filter(is_class2 == "Yes") %>%
          select(feat_33, feat_14, feat_15, feat_48, feat_25, feat_40) %>%
          mutate(multi = as.factor(feat_33 * feat_14 * feat_15 * feat_48 * feat_25 * feat_40),
            feat_33 = as.factor(feat_33), 
        feat_14 = as.factor(feat_14), 
        feat_15 = as.factor(feat_15),
        feat_48 = as.factor(feat_48),
        feat_25 = as.factor(feat_25),
        feat_40 = as.factor(feat_40)
        ))


summary(munged.data %>%
          filter(is_class2 == "No") %>%
        select(feat_33, feat_14, feat_15, feat_48, feat_25, feat_40) %>%
          mutate(multi = as.factor(feat_33 * feat_14 * feat_15 * feat_48 * feat_25 * feat_40),
                 feat_33 = as.factor(feat_33), 
                 feat_14 = as.factor(feat_14), 
                 feat_15 = as.factor(feat_15),
                 feat_48 = as.factor(feat_48),
                 feat_25 = as.factor(feat_25),
                 feat_40 = as.factor(feat_40)
          ))

class2.dat <- munged.data %>%
  filter(is_class2 == "Yes") %>%
  select(feat_33, feat_14, feat_15, feat_48, feat_25, feat_40)

rest.dat <- munged.data %>%
  filter(is_class2 == "No") %>%
  select(feat_33, feat_14, feat_15, feat_48, feat_25, feat_40)

mm_class2 <- model.matrix(~.^2, class2.dat)

fmm_class2 <- apply(mm_class2, 1:2, as.factor)


mm_rest <- model.matrix(~.^2, rest.dat)

fmm_rest <- apply(mm_rest, 1:2, as.factor)

summary(fmm_rest)


features <- munged.data %>% select(feat_1:feat_93)

combinations <- data.frame(cbind(model.matrix(~.^2, features), is_class2 = munged.data$is_class2))

grouped_combs <- combinations %>% group_by(is_class2) %>% summarise_each(funs(mean))
t_gc <- data.frame(t(grouped_combs))
t_gc <- cbind(t_gc,colnames(grouped_combs))

colnames(t_gc) <- c("No", "Yes", "Name")

data.frame(t_gc) %>% mutate(div = Yes / No, div2 = No / Yes) %>% filter(div2 > 1000)