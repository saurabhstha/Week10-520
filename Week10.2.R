# Assignment: binary-classifier-data.csv
# Name: Shrestha, Saurabh
# Date: 2021-05-23


library(foreign)
library(caTools)
library(dplyr)
install.packages('mlogit')
library(mlogit)

# Fit a Logistic Regression Model
# 2.a.Fit a logistic regression model to the binary-classifier-data.csv dataset
# Set working directory
setwd("C:/Users/Saurabh/Desktop/DSC 520/week10")

# 2.a. Load the data set
binary_df <- read.csv("C:/Users/Saurabh/Desktop/DSC 520/week10/binary-classifier-data.csv") 
head(binary_df)

new_model <- glm(label ~ x + y, data = binary_df, family = binomial())
summary(new_model)

# The dataset (found in binary-classifier-data.csv) contains three variables; 
# label, x, and y. The label variable is either 0 or 1 and is the output we want
# to predict using the x and y variables.
# What is the accuracy of the logistic regression classifier?
# 2.ii run the test data
binary_df$prediction <- fitted(new_model)
binary_df$predicted_label <- if_else(binary_df$prediction >= .50, 1, 0)

# validating the model using confusion matrix.
# useful tool for calibrating the output of a model and examining all possible
# outcomes of predictions
confmatrix <- table(Actual_Label = binary_df$label, Predicted_Label = binary_df$predicted_label)
(confmatrix[[1,1]] + confmatrix [[2,2]]) / sum(confmatrix)
# Based on the result there is 58.34% chances of accuracy in logistic regression.

# 2.ii Keep this assignment handy, as you will be comparing
# your results from this week to next week

