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


SplitWordsByCapitalLetter <- function(x){
   # Naive heuristic that assumes tokens with 
   # mixed casing can be decomposed into two or more words
   
   return(gsub("([a-z]+)([A-Z])", "\\1 \\2", x))


}
