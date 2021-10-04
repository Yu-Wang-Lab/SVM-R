############# Supported vector machine #############
# Date: 09/20/2021
# Author: Robert Madden
############# 1. Prepare data sets #################

## First specify the packages of interest
packages = c("readxl", "e1071",
             "caret")

## Now load or install & load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# setting the working directory
###########################################
#                                         #
#       Change to desired location        #
#                                         #
###########################################
setwd("C:/Users/Robert Ultrabook/SVM-R")
###########################################
#                                         #
#       Change to desired location        #
#                                         #
###########################################

  ## Create sub directory
  dir.create("pred_R")
  # library(readxl)
  ## Importing the data
  ###########################################
  #                                         #
  #       Change to desired file            #
  #                                         #
  ###########################################
  CP_data <- read.csv("C:/Users/Robert Ultrabook/SVM-R/data.csv")
  CP_data$Class <- as.factor(CP_data$Class)
  CP_data <- as.data.frame(CP_data[,-1])
  ###########################################
  #                                         #
  #       Change to desired file            #
  #                                         #
  ###########################################
  #library e1071
  ## Fitting Kernel SVM to the Training set
  set.seed(888)
  classifier = svm(Class ~ ., data = CP_data,
                   type = 'C-classification',
                   kernel = 'linear')
  
 
  # Saving the model.
  saveRDS(classifier, file = paste("pred_R", "/", "R", "_saved_model", sep = "", ".rds"))
  
  
  # Finding the top 100 variables and their coefficients.
  cat('SVM model case:\n')
  fit2 <- classifier
  w <- t(fit2$coefs) %*% fit2$SV                 # weight vectors
  w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
  w <- sort(w, decreasing = T)
  
  q = head(w, n = 100)    # the number of variables exported

  write.csv(q, file = paste("pred_R", "/", "R", "_Top 100 predictors", sep = "", ".csv"), row.names = TRUE)
  
  # normally I would use more clear variable names but for some reason this is
  # the only way I could get it to work.
