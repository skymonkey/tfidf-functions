CalculateTfIdf <- function(x){
   # Computes the term frequency - inverse document frequency
   # for a given set of vectors. 
   # 
   #  Args:
   #   x: Character vector. 
   #  
   #  Returns: 
   #   Matrix containing the tf-idf statistic for x.


   require(tm)
  
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