# SVM-R
SVM model creation for R

## Description of each file.

SVM_1_Modeling_testing is unrelated to the other two files. Is considered the base code to use as reference for making the others.

SVM_2_Prediction is training the model __without__ splitting the data. This means that the data will only train on itself. This file is used to make a csv with the top 100 most important variables.

SVM_3_Adaptation_external_test takes the top 100 variables and performs the final model creation.

## How to run.

The idea is that the data is processed already and separated out into two files. There will be the data file and the testset(ex). This is the holdout set and __not__ to be used to train the data.
You will need to change were the working directory is and where the files are located to make the system work. There are large blocks in the code to help show were changes may be needed. 
Once run is hit, folders will be created for each run ( = n). In the SVM_1_Modeling_testing, there can be as many runs/folders as you desire.

## Folder Output
model/pred/external files are generated after each run. They contain the output of each different model. The output for model and external should be: confusion matrix, K fold accuracy for all fold, the average k fold accuracy, the prediction output, the saved model, and the top 100 predictors. pred will have just the top 100 predictors and the saved model.
