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