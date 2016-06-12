TrainAndEvaluateClassifier <- function(x,y, testset){
  
  require(randomForest)
  tfidf <- CalculateTfIdf(x)
  fit <- randomForest(tfidf, as.factor(y),mtry=33, do.trace=T)
  test.tfidf <- CalculateTfIdf(testset)
  normalised <- normaliseTfIdfDataFrames(tfidf,test.tfidf)
  pred <- predict(fit, newdata=normalised, type='class')
  return(pred)


}

TrainRandomForestClassifier <- function(x,y, custom.mtry=NULL){
  # Trains a random forest classifier given the function 
  # given the inputs. 
  #
  #  Args: 
  #   x: Data frame of features.
  #   y: Vector of factors; the target classes.
  #   custom.mtry: Number of variables randomly sampled as candidates at each split. If null, default calculation 
  #         via the randomForest library will be used.
  require(randomForest)
#  if(!is.null(custom.mtry))  return(randomForest(x, y, do.trace=T)) else return(randomForest(x, y, mtry=custom.mtry,do.trace=T))
  fit <- randomForest(x, y, do.trace=T)
  plot(fit$err.rate)







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