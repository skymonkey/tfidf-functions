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