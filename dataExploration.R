GetSummaryStatisticsForTargetDataColumn <- function(x){
   # Calculates some descriptive statistics for the 
   # target data column in the set (specific helper function for this task).
   # 
   # Args:
   #   x: The predictor column for the task.
   #
   # Returns:
   #  A data frame containing corpus size, number of 
   #  examples of each class, number of 
   #  instances with no class.
   corpus.size <- length(x)
   nclasses <- nlevels(as.factor(x))
   no.class.label <- sum(is.na(x))
   message("Size of corpus is: ", corpus.size)
   message("Num of target classes: ", nclasses)
   message("Num of instances with no class label: ", no.class.label)



}