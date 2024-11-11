# Import the necessary libraries
library(tidyverse)
library(haven)
library(dplyr)
library(fastDummies)
library(nnet)
library(caret)
library(classpackage)
library(pROC)
library(ggplot2)
library(reshape2)
library(caret)
library(mice)
library(DMwR2)
library(e1071)
library(randomForest)
library(reshape2)
library(xtable)

# Set working directory
setwd("~/Capstone_Final/")
getwd()

# Call data preparation script
source("data_preparation.R")

# Call model training script
source("split_data.R")

# Call model evaluation script
source("logistic_regression_model.R")
source("random_forest_model.R")
source("svm_model.R")
