# Assignment: Thoraric surgery dataset
# Name: Shrestha, Saurabh
# Date: 2021-05-23

# Fit a Logistic Regression Model to Thoracic Surgery Binary Dataset.For this 
# problem, you will be working with the thoracic surgery data set from 
# the University of California Irvine machine learning repository. This dataset 
# contains information on life expectancy in lung cancer patients after surgery.
# The underlying thoracic surgery data is in ARFF format.
# This is a text-based format with information on each of the attributes. 
# You can load this data using a package such as foreign or by cutting and 
# pasting the data section into a CSV file.

library(foreign)
# Set working directory
setwd("C:/Users/Saurabh/Desktop/DSC 520/week10")

# a. Load the data set
thoraric_df <- read.arff("C:/Users/Saurabh/Desktop/DSC 520/week10/ThoraricSurgery.arff") 
head(thoraric_df)

# b.i Assignment Instructions:
# Fit a binary logistic regression model to the data set that predicts whether 
# or not the patient survived for one year (the Risk1Y variable) after the 
# surgery. Use the glm() function to perform the logistic regression. 
# See Generalized Linear Models for an example. Include a summary using the 
# summary() function in your results.

new_model <- glm(Risk1Yr ~ ., data = thoraric_df, family = 'binomial')
summary(new_model)

# According to the summary, which variables had the greatest effect on the survival rate?
# b.ii DGN, PRE5,PRE9T PRE17T, PRE30T variables had the greatest effect on the survival rate.

# b.iii To compute the accuracy of your model, use the dataset to predict the 
# outcome variable. The percent of correct predictions is the accuracy of your
# model. What is the accuracy of your model?

# split,train and test the data set
install.packages('caTools')
library(caTools)
library(dplyr)
# splits the data in the ratio mentioned in SplitRatio. 
# After splitting marks these rows as logical TRUE and the 
# remaining are marked as logical FALSE
split <- sample.split(thoraric_df, SplitRatio = 0.8)

# creates a training dataset named train1 
train1 <- subset(thoraric_df, split == 'TRUE')

test <- subset(thoraric_df, split == 'FALSE')

# running the test data via model
test_run <- predict(new_model, test, type = 'response')

# running the train1 data via model
test_run <- predict(new_model, train1, type = 'response')


# validating the model using confusion matrix.
# useful tool for calibrating the output of a model and examining all possible
# outcomes of predictionsconfmatrix = table(Actual_Value =thoraric_df$Risk1Yr, Predicted_Value = test > 0.50)

confmatrix = table(Actual_value=thoraric_df$Risk1Yr, Predicted_value = test > 0.50)
confmatrix


(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

