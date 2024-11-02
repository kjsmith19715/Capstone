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
training_data <- read_csv("training_data.csv")
test_data <- read_csv("test_data.csv")

# Fit logistic regression model
m1 <- glm(diagnosisAD ~ CDRSB + ADAS11 + MMSE + RAVLT_immediate,
          data = training_data, family = "binomial")
summary(m1)

# Check for outliers
car::outlierTest(m1)

# Check the significance of the model
reduced <- glm(diagnosisAD ~ 1, data = training_data, family = "binomial"(link="logit")) 
anova_result <- anova(reduced, m1, test = "LRT")

# Print the significance of the model
# Convert to a data frame for easier manipulation and safe to a file for generating a latex table
anova_df <- as.data.frame(anova_result)
saveRDS(anova_df, file = "anova_df.rds")

# Generate predicted probabilities using the test data set
test_data$predicted_probability <- predict(m1, newdata = test_data, type = "response")
view(test_data)

# Create ROC curve
roc_curve <- roc(test_data$diagnosisAD, test_data$predicted_probability)
saveRDS(roc_curve, file = "roc_curve_glm.RDS")

# Calculate AUC
auc_value <- auc(roc_curve)
print(paste("AUC - CSF Biomarkers:", auc_value))
write(paste("AUC - CSF Biomarkers:", auc_value), file = "auc_output_glm.csv")

# Convert predicted probabilities to predicted classes
predicted_classes <- ifelse(test_data$predicted_probability > .2, 1, 0)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$diagnosisAD))
saveRDS(confusion_matrix, file = "confusion_matrix_glm.rds")
print(confusion_matrix)
