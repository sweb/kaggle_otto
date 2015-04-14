View(raw.data %>% select(-id) %>% group_by(target) %>% summarise_each(funs(mean)))

ana.data <- munged.data %>% select(is_class1:is_class9) %>% 
  mutate(is_class1 = ifelse(is_class1 == "Yes", 1, 0),
         is_class2 = ifelse(is_class2 == "Yes", 1, 0),
         is_class3 = ifelse(is_class3 == "Yes", 1, 0),
         is_class4 = ifelse(is_class4 == "Yes", 1, 0),
         is_class5 = ifelse(is_class5 == "Yes", 1, 0),
         is_class6 = ifelse(is_class6 == "Yes", 1, 0),
         is_class7 = ifelse(is_class7 == "Yes", 1, 0),
         is_class8 = ifelse(is_class8 == "Yes", 1, 0),
         is_class9 = ifelse(is_class9 == "Yes", 1, 0))

ana.data %>% summarise_each(funs(mean))

ggplot(data = raw.data) +
  theme_bw() +
  geom_bar(aes(x = target)) +
  xlab("Product classes") +
  ylab("Number of products")


no_vector <- rep("No", nrow(validation.data))
no_vector <- as.factor(no_vector)
levels(no_vector) <- c("No", "Yes")

valid.data <- validation.data %>% select(is_class1:is_class9)
accus <- c()
for (i in seq(1,9)) {
  accus <- c(accus,confusionMatrix(no_vector, valid.data[,i])$overall[1])
}

confusionMatrix(no_vector, validation.data$is_class9)


accuracy.data <- read.csv("observations/accu_logreg.csv", header = TRUE, sep = ",", na.strings = "")
diff.data <- rbind(cbind(type = "Baseline", accuracy.data %>% select(-accus), accus),
  cbind(type = "Regression", accuracy.data  %>% select(-accus), accus = accuracy.data$accus - accus)
                   )

ggplot(data = diff.data, aes(y=accus)) +
  theme_bw() +
  geom_bar(stat="identity", aes(x = Class, y=accus, fill=type), position="stack") +
  scale_fill_brewer(type = "qual", palette="Blues")

write.csv(diff.data, file = "observations/accu_comparison.csv", quote = TRUE, row.names = TRUE)