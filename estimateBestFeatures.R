PlotOOB <- function(x, feature.importance, target){
  # Iteratively train a random forest classifier
  # using different numbers of important features.
  # The OOB is captured at each stage and at the end
  # the number of features is plotted against OOB.
  # Generates a sequence from 50 to 500 (increasing by intervals of
  # 50). Each interval represents the number of features which
  # will be used to train the model in each iteration.
  #
  # Args:
  #  x: Data frame of features with which to to train the model.
  #
  #  feature.importance: A data frame comprising all the 
  #                     features contained in x, but ranked in order of 
  #                     decreasing importance. 
  #  
  require(randomForest)

  num.features <- ncol(x)
  sequence <-  ifelse(num.features<=500, seq(from=50, to=num.features, by=10), seq(from=50, to=500, by=50))
  steps <- matrix(c(10,sequence))
  list.of.feature.data.frames <- (apply(steps,1,function(y) feature.importance[1:y,]))
  list.of.input.data.frames <- lapply(list.of.feature.data.frames, function(y) x[,which(colnames(x) %in% y[,1])])
  list.outputs <- lapply(list.of.input.data.frames, function(y) capture.output(randomForest(y, as.factor(target), do.trace=T)))
  oob <-unlist( lapply(list.outputs, function(y)  gsub(".*?(\\d{,2}(\\.\\d{,2})?)\\%","\\1",tail(y, n=7)[1])))
                                         
  plot(oob, type='l')
  return(data.frame("num_top_features"=steps, "oob"=oob))

}


GetVariableImportance <- function(x){
  # Takes a random forest model object and returns the
  # constituent variables as a data frame, ranked in
  # order of decreasing importance.
  #
  #  Args:
  #   x: Model object trained using the randomForest package.
  #
  #  Returns:
  #   data frame containing the model variables ranked in
  #   order of decreasing importance.
  #
  imp <- x$importance
  imp.df <- data.frame(row.names(imp), as.numeric(imp))
  imp.df <- imp.df[order(-imp.df[,2]),]
  row.names(imp.df) <- c(1:nrow(imp))
  return(imp.df)

}


OptimiseNumberOfFeatures <- function(x,features,imp,target){

   oob.error <- PlotOOB(features, imp, target)
   return(oob.error[order(oob.error[,2]),])
}

OptimiseModel <- function(x,features,target){

   imp <- GetVariableImportance(x)
   opt.num <- OptimiseNumberOfFeatures(x,features,imp,target)[1,1]
   best.features <- imp[1:opt.num,]
   features <- features[,which(colnames(features) %in% best.features[,1])]
   best.fit <- tuneRF(features, as.factor(target),doBest=T)
   return(list(model=best.fit,features=features))


}


PredictClasses <- function(x, model.features,test.features, titles){

  features <- normaliseTfIdfDataFrames(model.features,test.features)
  p <- as.matrix(predict(x, newdata=features, type='class'))
  predictions <- p[,1]
  results <- cbind(predictions,titles)
  
  return(results)
  


}