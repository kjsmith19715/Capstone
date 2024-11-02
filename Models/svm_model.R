
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
saveRDS(roc_curve, file = "roc_curve_svm.RDS")

# Calculate AUC
auc_value <- auc(roc_curve)
print(paste("AUC", auc_value))
write(paste("AUC:", auc_value), file = "auc_output_svm.csv")

# Convert predicted probabilities to predicted classes
predicted_classes <- ifelse(test_data$predicted_probability > .3, 1, 0)

# Evaluate the model using a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$diagnosisAD))
print(confusion_matrix)

# Convert predicted probabilities to predicted classes
predicted_classes <- ifelse(test_data$predicted_probability > .2, 1, 0)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$diagnosisAD))
saveRDS(confusion_matrix, file = "confusion_matrix_svm.rds")
print(confusion_matrix)