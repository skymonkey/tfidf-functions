TrainAndEvaluateClassifier <- function(x,y, testset){
  
  require(randomForest)
  tfidf <- CalculateTfIdf(x)
  fit <- randomForest(tfidf, as.factor(y),mtry=33, do.trace=T)
  test.tfidf <- CalculateTfIdf(testset)
  normalised <- normaliseTfIdfDataFrames(tfidf,test.tfidf)
  pred <- predict(fit, newdata=normalised, type='class')
  return(pred)


}