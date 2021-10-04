# outside data example

############# Supported vector machine #############
# Date: 09/20/2021
# Author: Robert Madden
############# #################### #################

## First specify the packages of interest
packages = c("readxl", "e1071",
             "caret", "caTools",
             "pROC", "rpart",
             "dplyr")

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
setwd("C:/Users/Robert Ultrabook/SVM-R")


## Create sub directory
dir.create(paste("external_R", sep = ""))
# library(readxl)
## Importing the data

# Loading the top 100 features and trimming the original data
# Making a vector of the top 100 from prediction
original_csv_top100_features = read.csv("C:/Users/Robert Ultrabook/SVM-R/pred_R/R_Top 100 predictors.csv")
top100 = original_csv_top100_features[, 1]
top100 <- as.vector(top100)

# Making"internal" data set. Used to create the final model.
dat = read.csv("C:/Users/Robert Ultrabook/SVM-R/data.csv")
dat_top100_all_columns = subset(dat, select = c("Sample", "Class", top100))
# Making class into a factor
dat_top100_all_columns$Class <- as.factor(dat_top100_all_columns$Class)
dat_top100 <- as.data.frame(dat_top100_all_columns[,-1])

# Making "external" data set. Used to test the final model.
external_data = read.csv("C:/Users/Robert Ultrabook/SVM-R/test_set(ex).csv")
external_data_top100_all_columns = subset(external_data, select = c("Sample", "Class", top100))
# Making class into a factor
external_data_top100_all_columns$Class <- as.factor(external_data_top100_all_columns$Class)
external_data_top100 <- as.data.frame(external_data_top100_all_columns[,-1])


# Creating excel csv of the trimmed data.
write.csv(dat_top100_all_columns, file = paste("external_R", "/", "R", "_data_top100", sep = "", ".csv"), row.names = FALSE)
write.csv(external_data_top100_all_columns, file = paste("external_R", "/", "R", "_external_data_top100", sep = "", ".csv"), row.names = FALSE)


#library e1071
## Fitting Kernel SVM to the trimmed internal data.
classifier = svm(Class ~ ., data = dat_top100,
                 type = 'C-classification',
                 kernel = 'linear')

#### Prediction
y_pred = predict(classifier, newdata = external_data_top100[-1])

# Making the Confusion Matrix
cm_tosave = confusionMatrix(y_pred, external_data_top100$Class, positive = "1")

# Plotting the confusion matrix
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Negative', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Positive', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Negative', cex=1.2, srt=90)
  text(140, 335, 'Positive', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
} 

# Saving the confusion matrix
jpeg(filename = paste("external_R", "/", "R", "_Confusion_Matrix", ".jpeg", sep = ""), width = 4.25, height = 4.25, units = "in", res = 300)
q = draw_confusion_matrix(cm_tosave)
print(q)
dev.off()


#### K fold validation
# library(caret)
# in creating the folds we specify the target feature (dependent variable) and # of folds
folds = createFolds(dat_top100$Class, k = 10)

# in cv we are going to applying a created function to our 'folds'
cv = lapply(folds, function(x) { # start of function
  # in the next two lines we will separate the Training set into it's 10 pieces
  training_fold = dat_top100[-x, ] # training fold =  training set minus (-) it's sub test fold
  test_fold = dat_top100[x, ] # here we describe the test fold individually
  # now apply (train) the classifier on the training_fold
  classifier = svm(formula = Class ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'linear')
  
  # next step in the loop, we calculate the predictions and cm and we equate the accuracy
  # note we are training on training_fold and testing its accuracy on the test_fold
  y_pred = predict(classifier, newdata = test_fold[-1])
  cm = table(test_fold[, 1], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

# Measuring the accuracy of the kfolds.
accuracy = 100 * round(mean(as.numeric(cv)), digits = 4)

# Saving the accuracy of each of the kfolds.
write.csv(cv, file = paste("external_R", "/", "R", "_K_fold_accuracy_all", ".csv", sep = ""))

# Saving the accuracy of the avearage of the kfolds.
write.table(accuracy, file = paste("external_R", "/", "R", "_K_fold_average", ".txt", sep = ""))

# Saving the final model
saveRDS(classifier, file = paste("external_R", "/", "R", "_Final_SVM_Model", ".rds", sep = ""))


