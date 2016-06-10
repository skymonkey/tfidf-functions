normaliseTfIdfDataFrames <- function(x, y){
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