

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




predict_data <- function(data, 
                         train.class1, train.class2, train.class3, 
                         train.class4, train.class5, train.class6, 
                         train.class7, train.class8, train.class9) {
  
  prob.class1 <- predict(train.class1, data, type="prob")
  prob.class2 <- predict(train.class2, data, type="prob")
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
                 prob.class9$Yes)
  
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
  
  res <- cbind( id = data %>% id, pred)
  return (res)
}


create_submission <- function(train.class1, train.class2, train.class3, 
                              train.class4, train.class5, train.class6, 
                              train.class7, train.class8, train.class9) {
  data <- read.csv("data/test.csv", header = TRUE, sep = ",", na.strings = "")
  predictions <- predict_data(data, train.class1, train.class2, train.class3, 
                              train.class4, train.class5, train.class6, 
                              train.class7, train.class8, train.class9)
  print(summary(predictions))
  write.csv(predictions, file = "result/submission.csv", 
            quote = FALSE, row.names = FALSE)
}