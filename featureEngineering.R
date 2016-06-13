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
  require(randomForest, quietly=TRUE)

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


GetRankedFeatures <- function(features,imp,target){
  # Gets and returns a data frame containing the
  # optimal number of features required to produce a minimal OOB.
  #
  # Args:
  #  features: input features to model
  #  imp: variable importance for model.
  #  target: vector containing target classes for model.
  #
  # Returns:
  #  Data frame containing the ranked features   
   oob.error <- PlotOOB(features, imp, target)
   return(oob.error[order(oob.error[,2]),])
}



CalculateTfIdf <- function(x){
   # Computes the term frequency - inverse document frequency
   # for a given set of vectors. 
   # 
   #  Args:
   #   x: Character vector. 
   #  
   #  Returns: 
   #   Matrix containing the tf-idf statistic for x.


   require(tm, quietly=TRUE)
  
   corpus <- Corpus(VectorSource(x))
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, removeWords, stopwords("en"))
   

   tf <- DocumentTermMatrix(corpus)
   tf <- removeSparseTerms(tf, .999)
   tf <- as.matrix(tf) 

   idf <- log(length(x)/colSums(sign(tf)))
   tf.idf <- as.data.frame(t(apply(tf,1, function(y) y * idf)))
   return(tf.idf)


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

NormaliseTfIdfDataFrames <- function(x, y){
   # Normalises two tf.idf dataframes so they contain 
   # the same terms (same column names). Any terms which are
   # added have weights set to 1e-10 (arbitrarily small value).
   # Normalisation is necessary so that  data in one frame can 
   # be used with a model trained on the other. 
   #
   #  Args:
   #    x: The data frame the model was trained on. 
   #    y: The data frame to be used with the trained model 
   #   
   #  Returns:
   #   A data frame containing all the values of y, but padded to include
   #   all missing terms which are included in x. These missing terms
   #   will have a default weight of 1e-10.
   missing.terms <- colnames(x[,which(!colnames(x) %in% colnames(y))])
   weight <- 1e-10
   missing.term.matrix <- matrix(weight, nrow = nrow(y), ncol = length(missing.terms))
   colnames(missing.term.matrix) <- missing.terms
   rownames(missing.term.matrix) <- rownames(y)
   normalised <- as.DocumentTermMatrix(cbind(y[, which(colnames(y) %in% colnames(x))], missing.term.matrix), weighting=weightTfIdf)
   return(normalised)

}

orderVariablesByImportance <- function(x){
  # Takes a random forest model and returns the variables
  # as a data.frame ordered by decreasing importance. 
  #
  #  Args: 
  #   x: Random forest model trained with randomForest package. 
  #
  #  Returns:
  #   Data frame containing the model variables ranked by decreasing
  #   importance. 
  
  var.importance <- importance(x)
   
  df <- data.frame(variables = row.names(var.importance), importance=var.importance[,1])
  return(df[order(-df$importance),])

}