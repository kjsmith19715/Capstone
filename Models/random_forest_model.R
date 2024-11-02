library(randomForest)
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

# Train the Random Forest model
rf_model <- randomForest(diagnosisAD ~ CDRSB + ADAS11 + MMSE + RAVLT_immediate, 
                         data = training_data, ntree = 100)
print(rf_model)

# Calculate predicted probability
test_data$predicted_probability <- predict(rf_model, newdata = test_data, type = "response")
view(test_data)

# Create ROC curve
roc_curve <- roc(test_data$diagnosisAD, test_data$predicted_probability)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve - CSF Biomarkers", col = "maroon")

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
heatmap_data <- as.data.frame(confusion_matrix$table)
colnames(heatmap_data) <- c("Predicted", "Actual", "Freq")
heatmap_data$Actual <- factor(heatmap_data$Actual, levels = c(1, 0))
heatmap_data$Predicted <- factor(heatmap_data$Predicted, levels = c(0, 1))

ggplot(heatmap_data, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "maroon") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Confusion Matrix Heatmap")















# Make predictions on the test set
predictions <- predict(rf_model, newdata = Analysis_Test_Data_D1D2)

# Create a confusion matrix
confusion_matrix <- table(Analysis_Test_Data_D1D2$diagnosisAD, predictions)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Plot variable importance
importance(rf_model)
varImpPlot(rf_model)

# Plot the confusion matrix
conf_matrix_table <- as.data.frame(confusion_matrix$table)

# Reshape the data for ggplot2
conf_matrix_table <- melt(conf_matrix_table)

# Create the plot
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = value), color = "black") +
  theme_minimal() +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted")
