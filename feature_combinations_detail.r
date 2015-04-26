setwd("C:/dev/repositories/R/kaggle_otto")
source("init_ws.r")

class2_accus <- read.csv("observations/feature_combinations.csv") %>% select(-X)

require(tidyr)

vis_class2_accus <- class2_accus %>% 
  filter (validation > 0.847) %>%
  mutate(feat = paste("feat", feat, sep="_")) %>%
  gather("type", "accuracy", 2:3)

ggplot(vis_class2_accus, aes(x=feat, y=accuracy, fill=type)) +
  theme_classic() +
  geom_bar(stat="identity", position="dodge")

# Best three feature-combinations (until 70): 64, 70, 9

require(doParallel)

c1 <- makeCluster(2)
registerDoParallel(c1)

i <- 70

engineered.data <- normalize(addFeatureCombination(i, munged.data))

split.data <- createDataPartition(engineered.data$is_class2, p = 0.6, list = FALSE)
t.data <- engineered.data[split.data, ]
tmp.test.data <- engineered.data[-split.data, ]

split.test <- createDataPartition(tmp.test.data$is_class2, p = 0.5, list = FALSE)

v.data <- tmp.test.data[split.test,]
ts.data <- tmp.test.data[-split.test,]

control.config <- trainControl(method = "repeatedcv", repeats = 1,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)

d <- data.frame()

for (j in seq(1:92)) {
  a <- Sys.time()
  current <- j + 93
  print(current)
  logreg.tmp.train <- train(is_class2 ~ .,
                            data = t.data %>% select(1:93,current, is_class2),
                            method = "glm",
                            metric = "ROC",
                            trControl = control.config)
  print (Sys.time())
  print (Sys.time() - a)
  
  logreg.tmp.pred_train <- predict(logreg.tmp.train, t.data  %>% select(1:93,current, is_class2))
  tmp.train.acc <- confusionMatrix(logreg.tmp.pred_train, t.data$is_class2)$overall[1]
  
  logreg.tmp.pred <- predict(logreg.tmp.train, v.data %>% select(1:93,current, is_class2))
  tmp.val.acc <- confusionMatrix(logreg.tmp.pred, v.data$is_class2)$overall[1]
  d <- rbind(d, cbind(feat = j, train = tmp.train.acc, validation = tmp.val.acc))
  print(paste("Train:", tmp.train.acc, "- Validation:", tmp.val.acc))
}

d %>% arrange(desc(validation))

# 181, 116, 171, 180, 122, 125, 177, 94, 124, 146, 154, 168