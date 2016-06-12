TrainModel <- function(x,target){

features <- CleanDatasetAndGenerateFeatures(x)
fit <- randomForest(features, as.factor(target), do.trace=F)
message("Estimating optimal number of features....")
imp <- GetVariableImportance(fit)
oob.error <- PlotOOB(features,imp, target)
oob.error[order(oob.error[,2]),]
best <- OptimiseModel(fit,features,target)
return(best)
#best$model
#best.OOB <- ExtractOOBFromCapturedTrainingOutput(capture.output(best$model))
#message("In this iteration the best model produced an OOB error of: ", best.OOB)
#message("Predicting the classes of the sub test set")
#features <- CalculateTfIdf(test$descriptions)
#message("Baseline OOB: ",baseline.OOB, "\nBest OOB with	model: ", best.OOB)
}