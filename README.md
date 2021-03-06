# Contents
* README.md - this file
* Answers.md - answers to the task question.
* dataPreparation.R - source code relating to data prepartion
* featureEngineering.R - source code relating to feature extraction and manipulation.
* trainModel.R - source code relating to training the classifier. 
* data.json - the task data.
* demo.R - The appilcatin script that executes all the steps necessary to train the models and make predictions.


# Specifications and Dependencies
* This prototype demo was developed using R version 3.3.0 with the following R package dependencies
  * randomForest
  * tm
  * jsonlite
  * textcat
  * caTools
* R can be started from the command line by simply typing: R and hitting return.
* These packages can be installed from within the R console using install.packages("nameOfPackageHere")
* The prototype demonstration can be run from within R by calling source("demo.R") from the console prompt.
* The demo can also be run without explicitly entering the R console, by typing Rscript demo.R from the terminal.
* All output pertaining to the demo is printed to the console:
    *  Baseline OOB error
    *  Best OOB after optimisatin
    *  Model confusion matrices.
* The predictions for the unlabelled classes generated by the two models are written to file in /tmp
    * description.model.predictions
    * title.model.predictions
* In both instances the predictions include the predicted class and the job title, regardless of whether the prediction was made based on analysis of the description or of the title. 
* Please make sure all source and data files are in the same directory when running.