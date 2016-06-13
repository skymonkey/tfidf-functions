TrainModel <- function(x,target){
  # Cleans the data and generates features (tf-idf). 
  # Optimises the feature set and also optimises the
  # model parameters, returning the best fit model.
  #
  #  Args:
  #   x: Column vector containing the training data set. 
  #   target: Column vector containing the target labels corresponding
  #           to the data in x.
  #
  #  Return:
  #   A list containing the best fit model and the corresponding
  #   optimised feature set used to train it. 

  features <- CleanDatasetAndGenerateFeatures(x)
  fit <- randomForest(features, as.factor(target), do.trace=F)
  message("Estimating optimal number of features....")
  imp <- GetVariableImportance(fit)
  oob.error <- PlotOOB(features,imp, target)
  oob.error[order(oob.error[,2]),]
  best <- OptimiseModel(fit,features,target)
  return(best)
 
}

ExtractOOBFromCapturedTrainingOutput <- function(x){
   # Extracts the final OOB error from the captured output
   # logs generated during random forest training. 
   # Note: this is a custom extraction function 
   # implemented solely for the mihiro task. 
   #
   #   Args:
   #    x: Vector containing the captured output from function randomForest(...)
   #
   #  Returns:
   #    The final OOB error from the captured output.

    return(gsub(".*?(\\d{,2}\\.\\d{,2})\\%","\\1",tail(x, n=7)[1]))

}


OptimiseModel <- function(x,features,target){
  # Optimises the model by calculating the optimum
  # number of important features in order to minimise
  # OOB error. These optimal features are then used to
  # train a number of different classifiers, each training
  # iteration seeks to fine-tune the mtry parameter (which
  # governs the number of variables randomly sampled as candidates at each
  # split).
  #
  # Args:
  #   x: A random forest classifier trained using the randomForest library.
  #   features: data frame containing the predictor features used to train x.
  #   target: vector containing the target classes associated with the features.
  #
  # Returns:
  #   A list containing the optimised model (labelled' best.fit') and the
  #   optimised features used to generated it (labelled 'features')

   imp <- GetVariableImportance(x)
   opt.num <- GetRankedFeatures(features,imp,target)[1,1]
   best.features <- imp[1:opt.num,]
   features <- features[,which(colnames(features) %in% best.features[,1])]
   best.fit <- tuneRF(features, as.factor(target),doBest=T)
   return(list(model=best.fit,features=features))


}




PredictClasses <- function(x, model.features,test.features, titles){
  # Predicts classes based on some input features. 
  #
  # Args: 
  #  x: The classification model.
  #  model.features: data.frame comprising the features used to train the model.
  #  test.features: input features for prediction.
  #  titles: Just for this task, the titles associated with the 
  #          job entries whose levels are to be predicted.
  #
  # Returns:
  #   Predicted classes and their corresponding titles for comparison.
  
  features <- NormaliseTfIdfDataFrames(model.features,test.features)
  p <- as.matrix(predict(x, newdata=features, type='class'))
  predictions <- p[,1]
  results <- cbind(predictions,titles)

  return(results)



}
