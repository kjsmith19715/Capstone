# Set working directory
setwd("~/Capstone_Final/")
getwd()

# Retrieve data
training_data <- read_csv("training_data.csv")
test_data <- read_csv("test_data.csv")
training_data$diagnosisAD = factor(training_data$diagnosisAD)
test_data$diagnosisAD = as.factor(test_data$diagnosisAD)

# Train the SVM model
svm_model <- svm(diagnosisAD ~ CDRSB + ADAS11 + MMSE + RAVLT_immediate, 
                 data = training_data,
                 probability = TRUE,
                 kernel = "radial",
                 type = "C-classification",
                 class.weights = class_weights) # weights calculated in 'split_data'

# Print model summary
summary(svm_model)

# Check for overfitting and underfitting 
# Calculate training accuracy
train_pred <- predict(svm_model, training_data)
test_pred <- predict(svm_model, test_data)
train_accuracy <- mean(train_pred == training_data$diagnosisAD)
test_accuracy <- mean(test_pred == test_data$diagnosisAD)

# Generate predicted probabilities using the test data set
svm_predictions <- predict(svm_model, newdata = test_data, probability = TRUE)

# Extract predicted probabilities for each class
pred_probs <- attr(svm_predictions, "probabilities")
  
# ROC CURVES-----------------------------------------------------------------
  
# Assuming diagnosisAD is a factor with levels corresponding to your categories
true_labels <- test_data$diagnosisAD
  
# Create an empty list to hold ROC curves
roc_curves <- list()
colors <- rainbow(length(levels(true_labels)))  # Generate distinct colors for each class
  
# Generate ROC curves for each class
for (i in seq_along(levels(true_labels))) {
    class <- levels(true_labels)[i]
    
   # Create a binary response (1 for the class, 0 for all others)
    binary_labels <- as.numeric(true_labels == class)
    
    # Compute and store the ROC curves
    roc_curve <- roc(binary_labels, pred_probs[, class], levels = c(0, 1))
    roc_curves[[class]] <- roc_curve
    
    # Plot the ROC curves
    if (i == 1) {
      plot(roc_curve, col = colors[i], 
           main = "ROC Curves - Support Vector Machine", 
           lwd = 2,
           legacy.axes = TRUE)
    } else {
      # Add subsequent ROC curves
      lines(roc_curve, col = colors[i], lwd = 2)
    }
  }
  category_names <- c("1-CN", "2-EMCI", "3-LMCI", "4-AD")
  legend("bottomright", 
         legend = category_names,   # Use category names
         col = colors,              # Use the same colors as in the plot
         lwd = 2)
  
  #-AUC-------------------------------------------------------------------------
  
  # Micro-average AUC: considers each individual prediction
  micro_roc <- multiclass.roc(test_data$diagnosisAD, pred_probs)
  micro_auc_svm <- auc(micro_roc)
  print(paste("Micro-average AUC - SVM:", micro_auc_svm))
  
  # Macro-average AUC: average of each class's AUC
  macro_auc_svm <- mean(sapply(roc_curves, auc))
  print(paste("Macro-average AUC - SVM:", macro_auc_svm))
  
  #-CONFUSION MATRIX------------------------------------------------------------
  
  # Convert predicted probabilities to predicted classes
  predicted_classes <- predict(svm_model, newdata = test_data)
  actual_classes <- test_data$diagnosisAD
  
  # Create a confusion matrix
  confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(actual_classes))
  saveRDS(confusion_matrix, file = "~/Capstone_Final/Outputs/confusion_matrix_svm.rds")
  print(confusion_matrix)
  
  # Calculate accuracy manually
  conf_matrix <- confusion_matrix$table
  accuracy_svm <- sum(diag(conf_matrix)) / sum(conf_matrix)
  print(paste("Accuracy - SVM:", round(accuracy_svm, 4)))
  
  # Calculate F1 score
  f1_scores <- sapply(levels(test_data$diagnosisAD), function(class) {
    TP <- confusion_matrix$table[class, class]
    FN <- sum(confusion_matrix$table[class, ]) - TP
    FP <- sum(confusion_matrix$table[, class]) - TP
    
    # Check if TP, FP, or FN are zero
    if (TP == 0) {
      return(0)  # Set F1 to 0 if there are no true positives
    }
    
    precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
    recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    
    f1 <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
    return(f1)
  })
  
  # Calculate Macro F1 Score
  macro_f1 <- mean(f1_scores, na.rm = TRUE)
  print(paste("Macro F1 Score:", macro_f1))
  
  # Convert confusion matrix to data frame
  heatmap_data <- as.data.frame(as.table(confusion_matrix))
  colnames(heatmap_data) <- c("Predicted", "Actual", "Freq")
  heatmap_data$Actual <- factor(heatmap_data$Actual, levels = c(1, 2, 3, 4))
  heatmap_data$Predicted <- factor(heatmap_data$Predicted, levels = c(4, 3, 2, 1))
  
  # Plot heatmap with ggplot2
  ggplot(heatmap_data, aes(x = Predicted, y = Actual, fill = Freq)) +
    geom_tile(color = "white") +  # Creates the heatmap cells with white borders
    scale_fill_gradient(low = "white", high = "lightgreen") +  # Define color gradient
    geom_text(aes(label = Freq), color = "black") +  # Display cell values
    labs(title = "Confusion Matrix Heatmap - SVM", x = "Predicted", y = "Actual") +
    theme_dark()
