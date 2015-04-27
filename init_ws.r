require(dplyr)
require(ggplot2)
require(caret)

setwd("C:/dev/repositories/R/kaggle_otto")

source("otto_functions.r")

raw.data <- read.csv("data/train.csv", header = TRUE, sep = ",", na.strings = "")
munged.data <- munge_data(raw.data)


set.seed(42)
split.data <- createDataPartition(munged.data$is_class1, p = 0.6, list = FALSE)
train.data <- munged.data[split.data, ]
tmp.test.data <- munged.data[-split.data, ]
set.seed(42)
split.test <- createDataPartition(tmp.test.data$is_class1, p = 0.5, list = FALSE)

validation.data <- tmp.test.data[split.test,]
test.data <- tmp.test.data[-split.test,]

control.config <- trainControl(method = "repeatedcv", repeats = 3,
                               summaryFunction = twoClassSummary,
                               classProbs = TRUE)