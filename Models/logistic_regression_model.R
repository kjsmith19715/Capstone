# Set working directory
setwd("~/Capstone_Final/")
getwd()

# Retrieve data
training_data <- read_csv("training_data.csv")
test_data <- read_csv("test_data.csv")

# Fit the multinomial logistic regression model
training_data$diagnosisAD = as.factor(training_data$diagnosisAD)
test_data$diagnosisAD = as.factor(test_data$diagnosisAD)

#METHOD 1---------
m1 <- multinom(diagnosisAD ~ CDRSB + ADAS11 + MMSE + RAVLT_immediate,
                 data = training_data,
                 class.weights = class_weights) # Weights calculated in 'split_data'
summary(m1)
round(exp(coefficients(m1)), 2)

# Obtain global and model results 
car::Anova(m1, type = 3)

# Print confidence intervals
multinom_ci <- round(confint(m1),2)

# Detect overfitting and underfitting

trainPred <- predict(m1, newdata = training_data, type = "class")
testPred <- predict(m1, newdata = test_data, type = "class")

# Calculate accuracy for training and test data
trainAccuracy <- mean(trainPred == training_data$diagnosisAD)
testAccuracy <- mean(testPred == test_data$diagnosisAD)
  
  
# ROC CURVES-----------------------------------------------------------------

# Get predicted probabilities for the test data
pred_probs <- predict(m1, newdata = test_data, type = "probs")

# Assuming diagnosisAD is a factor with levels corresponding to your categories
true_labels <- test_data$diagnosisAD

# Create an empty list to hold ROC curves
roc_curves <- list()
colors <- rainbow(length(levels(true_labels)))  # Generate distinct colors for each class

# Generate ROC curves for each class
for (i in seq_along(levels(true_labels))) {
  class <- levels(true_labels)[i]
  cat("The value of class is:", class, "\n")
  # Create a binary response (1 for the class, 0 for all others)
  binary_labels <- as.numeric(true_labels == class)

  # Compute and store the ROC curve
  roc_curve <- roc(binary_labels, pred_probs[, class], levels = c(0,1))
  roc_curves[[class]] <- roc_curve

  # Plot the ROC curves
  if (i == 1) {
    plot(roc_curve,
         col = colors[i],
         main = "ROC Curves - Multinomial Logistic Regression",
         lwd = 2,
         legacy.axes = TRUE)
  } else {
    lines(roc_curve,
          col = colors[i],
          lwd = 2)
  }
  # Add legend with matching colors
  category_names <- c("1-CN", "2-EMCI", "3-LMCI", "4-AD")
  legend("bottomright",
         legend = category_names,   # Use category names
         col = colors,              # Use the same colors as in the plot
         lwd = 2)
}
#   
#-AUC-------------------------------------------------------------------------
  
# Micro-average AUC: considers each individual prediction
micro_roc <- multiclass.roc(test_data$diagnosisAD, pred_probs)
micro_auc_minom <- auc(micro_roc)
print(paste("Micro-average AUC - Logistic Regression:", micro_auc_minom))
  
# Macro-average AUC: average of each class's AUC
macro_auc_minom <- mean(sapply(roc_curves, auc))
print(paste("Macro-average AUC - Logistic Regression:", macro_auc_minom))
  
#-CONFUSION MATRIX------------------------------------------------------------
  
# Convert predicted probabilities to predicted classes
predicted_classes <- predict(m1, newdata = test_data)
actual_classes <- test_data$diagnosisAD
  
# Create a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(actual_classes))
saveRDS(confusion_matrix, file = "~/Capstone_Final/Outputs/confusion_matrix_multinom.rds")
print(confusion_matrix)
  
# Calculate accuracy
accuracy_multinom <- confusion_matrix$overall['Accuracy']
  
# Convert confusion matrix to data frame
heatmap_data <- as.data.frame(as.table(confusion_matrix))
colnames(heatmap_data) <- c("Predicted", "Actual", "Freq")
heatmap_data$Actual <- factor(heatmap_data$Actual, levels = c(1, 2, 3, 4))
heatmap_data$Predicted <- factor(heatmap_data$Predicted, levels = c(4, 3, 2, 1))
  
# Plot heatmap with ggplot2
ggplot(heatmap_data, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +  # Creates the heatmap cells with white borders
  scale_fill_gradient(low = "white", high = "lightblue") +  # Define color gradient
  geom_text(aes(label = Freq), color = "black") +  # Display cell values
  labs(title = "Confusion Matrix Heatmap - Multinomial Logistic Regression", x = "Predicted Diagnosis", y = "Actual Diagnosis") +
  theme_dark()

