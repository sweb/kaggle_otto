createComparisonPlot <- function(primaryFeature, secondaryFeature, data){
  if (primaryFeature == secondaryFeature) {
    return ()
  }
  plot_data <- data %>% select(primaryFeature, secondaryFeature, is_class2)
  orig_colnames <- colnames(plot_data)
  colnames(plot_data) <- c("x1", "x2", "is_class2")
  
  pl <- ggplot(plot_data, aes(x=x1, y=x2, color=is_class2)) +
    theme_classic() +
    geom_point(alpha=0.7) +
    xlab(orig_colnames[1]) +
    ylab(orig_colnames[2]) 
  
  filename <- paste("plots/", primaryFeature, "_", secondaryFeature, ".png", sep="")
  
  ggsave(plot = pl, file=filename)
}

for (i in 1:93) {
  createComparisonPlot(9, i, train.data)
}


results <- list()
relevant_features <- list(6, 10, 14, 15, 20, 22, 32, 39, 40, 48, 50, 54, 56, 62, 64, 66, 70, 77, 79, 88)
length(results) <- length(relevant_features)
counter <- 1
for (i in relevant_features) {
  print(i)
  results[[counter]] <- evaluateFeaturePolynoms(9,i, train.data, validation.data,5)
  counter <- counter + 1
}

p <- evaluateFeaturePolynoms(9, 16, train.data, validation.data, 8)

degree_list <- list(3,3,3,3,2,3,3,4,6,2,3,3,4,4,4,2,3,3,4,4)

tmp <- createAllPolyFeatures(9, relevant_features, degree_list)

removeDuplicateFeatures <- function(df) {
  return(df %>% select(-contains(".")))
}

degree_list <- list(3,3,3,2,3,3,4,6,2,3,3,4,4,4,2,3,3,4,4)
new_poly_df <- data.frame()
for (i in relevant_features) {
  new_poly <- createPolyFeatures(munged.data %>% select(9, i), 3)
}
