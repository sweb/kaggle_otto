for (feat in 1:93) {
  for (i in 1:feat) {
    createComparisonPlot(feat, i, train.data)
  }
}

for (i in 1:93) {
  createComparisonPlot(48, i, train.data)
}

plot_data <- train.data %>% select(feat_1, is_class2)

ggplot(plot_data, aes(x = plot_data[,1], fill = plot_data[,2])) +
  theme_classic() +
  geom_bar(position = "dodge", binwidth = 5)

used_data <- train.data %>% mutate(feat_14_feat_40_feat_25 = feat_14 * feat_40 * feat_25)

colnames(used_data %>%select(103))

for (i in 1:93) {
  createComparisonPlot(103, i, used_data)
}

results <- list()
relevant_features <- list(48)
length(results) <- length(relevant_features)
counter <- 1
for (i in relevant_features) {
  a <- Sys.time()
  print(i)
  results[[counter]] <- evaluateFeaturePolynoms(64,i, train.data, validation.data,5)
  counter <- counter + 1
  print (Sys.time() - a)
}

p <- evaluateFeaturePolynoms(14, 40, train.data, validation.data, 5)
p
#85 higher degree
#88 higher degree
degree_list <- list(5)

tmp <- createAllPolyFeatures(25, relevant_features, degree_list)

removeDuplicateFeatures <- function(df) {
  return(df %>% select(-contains(".")))
}

degree_list <- list(3,3,3,2,3,3,4,6,2,3,3,4,4,4,2,3,3,4,4)
new_poly_df <- data.frame()
for (i in relevant_features) {
  new_poly <- createPolyFeatures(munged.data %>% select(9, i), 3)
}
