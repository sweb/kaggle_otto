for (i in 1:93) {
  createComparisonPlot(2, i, train.data)
}


results <- list()
relevant_features <- list(14, 15, 16, 40, 42, 44, 48, 54, 66, 67, 72, 88)
length(results) <- length(relevant_features)
counter <- 1
for (i in relevant_features) {
  a <- Sys.time()
  print(i)
  results[[counter]] <- evaluateFeaturePolynoms(25,i, train.data, validation.data,5)
  counter <- counter + 1
  print (Sys.time() - a)
}

p <- evaluateFeaturePolynoms(25, 67, train.data, validation.data, 5)
p
#85 higher degree
#88 higher degree
degree_list <- list(3,4,3,4,4,3,4,3,3,5,4,4)

tmp <- createAllPolyFeatures(25, relevant_features, degree_list)

removeDuplicateFeatures <- function(df) {
  return(df %>% select(-contains(".")))
}

degree_list <- list(3,3,3,2,3,3,4,6,2,3,3,4,4,4,2,3,3,4,4)
new_poly_df <- data.frame()
for (i in relevant_features) {
  new_poly <- createPolyFeatures(munged.data %>% select(9, i), 3)
}
