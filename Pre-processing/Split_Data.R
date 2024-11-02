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

# Set working directory
setwd("~/Capstone/")
getwd()

# Retrieve data
Data_D1D2 <- read_csv("Data_D1D2.csv")
view(Data_D1D2)

# Create a partition for training data
set.seed(123)
train_index <- createDataPartition(Data_D1D2$DX_bl, p = 0.7, list = FALSE)  # 70% for training

# Split the dataset into training and test sets
Training_Data_D1D2 <- Data_D1D2[train_index, ]
Test_Data_D1D2 <- Data_D1D2[-train_index, ]

# Check the dimensions of the training and test sets
cat("Training set dimensions:", dim(Training_Data_D1D2), "\n")
cat("Test set dimensions:", dim(Test_Data_D1D2), "\n")

#-----------BALANCE DATA----------------

# Function to perform undersampling
undersample_data <- function(data, target_class) {
  # Count the number of instances in the target (minority) class
  minority_count <- sum(data$DX_bl == target_class)
  
  # Select instances from the minority class
  minority_data <- data[data$DX_bl == target_class, ]
  
  # Select instances from the majority class, randomly
  majority_data <- data[data$DX_bl != target_class, ]
  
  # Randomly sample from the majority class to match the minority count
  majority_data_sampled <- majority_data[sample(nrow(majority_data), minority_count), ]
  
  # Combine the sampled majority class with the minority class
  balanced_data <- rbind(minority_data, majority_data_sampled)
  
  return(balanced_data)
}

# Perform undersampling to balance the training and test datasets
Training_Data_D1D2 <- undersample_data(Training_Data_D1D2, "AD")
Test_Data_D1D2 <- undersample_data(Test_Data_D1D2, "AD")

# Check the new distribution of the classes
Training_Data_D1D2 %>%
  count(DX_bl)
Test_Data_D1D2 %>%
  count(DX_bl)

# Create a binary indicator variable for MCI and AD diagnosis for both training and test data
Training_Data_D1D2 <- Training_Data_D1D2 %>%
  mutate(diagnosisAD = if_else(DX_bl == "AD", 1, 0)) %>%
  dummy_cols(select_columns = c("diagnosisAD"))

Test_Data_D1D2 <- Test_Data_D1D2 %>%
  mutate(diagnosisAD = if_else(DX_bl == "AD", 1, 0)) %>%
  dummy_cols(select_columns = c("diagnosisAD"))

# Write training and test data files
write.csv(Training_Data_D1D2, "training_data.csv", row.names = TRUE)
write.csv(Test_Data_D1D2, "test_data.csv", row.names = TRUE)
