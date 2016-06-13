LoadDataJson <- function(x){
  # Loads json from disk and returns a data frame.
  #
  #  Args:
  #   x: Character vector. Location of json on disk.
  #
  #  Returns:
  #   Data frame representation of the json located at x.
  require(jsonlite)
  return(fromJSON(x))


}

ExtractJobsInEN <- function(x){
  # Takes the task data, detects the language of the description and returns only
  # those job listings in English. 
  #
  # Args: 
  #   x: The task data frame. 
  #
  # Returns:
  #   A data frame containing all the job listings from the task data
  #   which are in English.
  require(textcat)
  x$lang <- textcat(x$description)
  return(x[which(x$lang == 'english'),])

}

ReplaceUnicodeEscapesWithSpaces <- function(x){
  # Matches escaped unicode sequences and replaces them
  # with a single space. 
  #
  #  Args:
  #   x: character vector.
  #  
  #  Returns:
  #   The same character vector, but with all escaped unicode
  #   character sequences replaced by single space. 
  #
  return(gsub("\\\\u[\\d\\w]{4}"," ", x, ignore.case=T))
}


PrepareMasterTrainingAndTestSets <- function(x, col.index){
  # Splits the original data set into training set 
  # (instances with a class label), and a test set
  # (instances with no class label).
  #
  #  Args:
  #    x: The task data set. A data frame.
  #    col.index: index of column to split set by.
  #
  #  Returns:
  #   A list containing two data sets labelled train and test.
  
  train <- x[!is.na(x[,col.index]),]
  test <- x[is.na(x[,col.index]),]
  return(list(train=train,test=test))

}


PrepareSubTrainingAndTestSets <- function(x){
  # Splits a fully labelled training set into
  # a training and a test subset. This allows
  # the performance of a model to be examined 
  # as the predicted classes can be compared 
  # against the given labels.
  #
  # Args: 
  #  x: Data frame containing fully labelled examples.
  #
  # Returns:
  #  A list containing two data frames labelled sub.train and 
  #  sub.test which split x in 0.8/0.2 ratio.

  require(caTools)
  sample <- sample.split(x[,1], SplitRatio=.8)
  sub.train <- subset(x, sample==T)
  sub.test <- subset(x, sample==F)
  return(list(sub.train=sub.train, sub.test=sub.test))


}

SplitDataFrameByFactor <- function(x, col.index){
  # Splits a data frame by some factor.
  #
  # Args:
  #   x: A data frame.
  #   col.index: Index of the column containing factors to split by.
  #
  # Returns:
  #  A list of data frames. Each data frame contains examples of 
  #  a single factor level from x[,col.index].
  return(split(x, x[,col.index]))


}

SplitWordsByCapitalLetter <- function(x){
   # Naive heuristic that assumes tokens with 
   # mixed casing can be decomposed into two or more words.
   # 
   # Args:
   #  x: Column vector containing the data to be cleaned.
   #
   # Returns:
   #  The same column vector but with all Unicode escape sequences
   #  replaced by spaces.
   
   return(gsub("([a-z]+)([A-Z])", "\\1 \\2", x))


}


CleanDatasetAndGenerateFeatures <- function(x){
  # Applies some simple data cleaning and generates  
  # the feature vectors (tf-idf). 
  # 
  # Args:
  #   x: Column vector containing the training data.
  #
  # Returns:
  #   Data frame containing the features for training (tf-idf).
   
  x<- ReplaceUnicodeEscapesWithSpaces(x)
  return(CalculateTfIdf(x))



}

GetSummaryStatisticsForTargetDataColumn <- function(x){
   # Calculates some descriptive statistics for the 
   # target data column in the set (specific helper function for this task).
   # Outputs stats to console for demo purposes.
   #
   # Args:
   #   x: The predictor column for the task.
   #
   corpus.size <- length(x)
   nclasses <- nlevels(as.factor(x))
   no.class.label <- sum(is.na(x))
   message("Size of corpus is: ", corpus.size)
   message("Num of target classes: ", nclasses)
   message("Num of instances with no class label: ", no.class.label)

}