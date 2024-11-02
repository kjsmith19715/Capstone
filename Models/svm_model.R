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
library(e1071)

# Set working directory
setwd("~/Capstone/")
getwd()

# Retrieve data
training_data <- read_csv("training_data.csv")
test_data <- read_csv("test_data.csv")

# Train the SVM model
svm_model <- svm(diagnosisAD ~ CDRSB + ADAS11 + MMSE + RAVLT_immediate + ABETA, 
                 data = training_data,
                 kernel = "radial", 
                 cost = 1.5)  # Adjust weights as needed
# Print model summary
summary(svm_model)

# Calculate predicted probability
test_data$predicted_probability <- predict(svm_model, newdata = test_data, type = "response")
view(test_data)

# Create ROC curve
roc_curve <- roc(test_data$diagnosisAD, test_data$predicted_probability)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve - CSF Biomarkers", col = "purple")

# Print AUC
print(paste("AUC - CSF Biomarkers:", auc(roc_curve)))

# Convert predicted probabilities to predicted classes
predicted_classes <- ifelse(test_data$predicted_probability > .3, 1, 0)

# Evaluate the model using a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$diagnosisAD))
print(confusion_matrix)

# Reshape the data for ggplot2
confusion_matrix_table <- as.data.frame(confusion_matrix$table)
confusion_matrix_table <- melt(confusion_matrix_table)

# Create the plot
# Plot the confusion matrix
heatmap_data <- as.data.frame(confusion_matrix$table)
colnames(heatmap_data) <- c("Predicted", "Actual", "Freq")
heatmap_data$Actual <- factor(heatmap_data$Actual, levels = c(1, 0))
heatmap_data$Predicted <- factor(heatmap_data$Predicted, levels = c(0, 1))

ggplot(heatmap_data, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Confusion Matrix Heatmap")

# ggplot(data = confusion_matrix_table, aes(x = Reference, y = Prediction)) +
#   geom_tile(aes(fill = value), color = "white") +
#   scale_fill_gradient(low = "white", high = "purple") +
#   geom_text(aes(label = value), color = "black") +
#   theme_minimal() +
#   labs(title = "Confusion Matrix", x = "Actual", y = "Predicted")