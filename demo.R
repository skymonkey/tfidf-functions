source("dataPreparation.R")
source("featureEngineering.R")
source("trainModel.R")
message("Scripts loaded.....")

# Load the data and output some stats
data <- LoadDataJson("data.json")
message("data.json loaded...")
GetSummaryStatisticsForTargetDataColumn(data$level)

# Prepare training and test sets
datasets <- PrepareMasterTrainingAndTestSets(data,1)
message("Data split into master training and test sets....")
train <- datasets$train
test <- datasets$test

# Feature engineering + baseline
features <- CalculateTfIdf(train$description)
message("Training baseline classifier using job descriptions....")
library(randomForest)
message("Training baseline classifier using job descriptions....")
baseline.fit <- randomForest(features, as.factor(train$level), do.trace=F)
baseline.OOB <- ExtractOOBFromCapturedTrainingOutput(capture.output(baseline.fit))
message("Baseline OOB error with default settings and no data cleaning: ", baseline.OOB)
message("Cleaning descriptions and generating new features...")

# Use only examples with descriptions in English.
train <- ExtractJobsInEN(train)

# Train model using job descriptions
best <- TrainModel(train$description, train$level)
message("Printing the model summary, including confustion matrix. Model trained on descriptions.")
best$model
best.OOB <- ExtractOOBFromCapturedTrainingOutput(capture.output(best$model))
message("In this iteration the best model produced an OOB error of: ", best.OOB)
message("Predicting the classes of the test set")
description.model.predictions <- PredictClasses(best$model, best$features, features, test$title)


# Train classifier using only the job titles.
message("Train with job titles instead.....")
best <- TrainModel(train$title, train$level)
message("Printing the model summary, including confustion matrix. Model trained on titles.")
best$model
best.OOB <- ExtractOOBFromCapturedTrainingOutput(capture.output(best$model))
message("In this iteration the best model produced an OOB error of: ", best.OOB)

features <- CalculateTfIdf(test$descriptions)
title.model.predictions <- PredictClasses(best$model, best$features, features, test$title)


# Write output to disk
write.csv(description.model.predictions, "/tmp/description.model.predictions.csv", row.names=F)
write.csv(title.model.predictions, "/tmp/title.model.predictions.csv", row.names=F)
