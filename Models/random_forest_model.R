setwd("~/Capstone_Final/")
getwd()

# Retrieve data
training_data <- read_csv("training_data.csv")
test_data <- read_csv("test_data.csv")
training_data$diagnosisAD = as.factor(training_data$diagnosisAD)
test_data$diagnosisAD = as.factor(test_data$diagnosisAD)

# Train the Random Forest model
rf_model <- randomForest(diagnosisAD ~ CDRSB + ADAS11 + MMSE + RAVLT_immediate, 
                         data = training_data, 
                         ntree = 100, 
                         drop = TRUE,
                         maxnodes = 30,
                         class.weights = class_weights) # weights calculated in 'split_data')
print(rf_model)

#---CHECK FOR UNDER AND OVER FITTING

# Predict on training and test sets
trainPred <- predict(rf_model, newdata = training_data)
testPred <- predict(rf_model, newdata = test_data)

# Calculate accuracy
trainAccuracy <- mean(trainPred == training_data$diagnosisAD)
testAccuracy <- mean(testPred == test_data$diagnosisAD)

# Display results
cat("Training Accuracy: ", trainAccuracy, "\n")
cat("Test Accuracy: ", testAccuracy, "\n")



#-------



# Make predictions on the test set
pred_probs <- predict(rf_model, newdata = test_data, type="prob")
  
# ROC CURVES-----------------------------------------------------------------
  
# Create one-vs-all ROC curves for each class
  roc_curves <- list()  # Store ROC curves
  colors <- rainbow(length(levels(true_labels)))  # Generate distinct colors for each class
  
for (class in colnames(pred_probs)) {
    
  # Create binary labels for one-vs-all comparison
  binary_labels <- as.numeric(test_data$diagnosisAD == class)
    
  # Compute and store the ROC curves
    roc_curve <- roc(binary_labels, 
                     pred_probs[, class], 
                     levels = c(0, 1), 
                     direction = "<")
    roc_curves[[class]] <- roc_curve
  }
  
# Plot the ROC curves
plot(roc_curves[[1]], 
     col = colors[1], 
     main = "ROC Curves - Random Forest",
     legacy.axes = TRUE)
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], add = TRUE, col = colors[i])  
 }
  
# Add a legend
category_names <- c("1-CN", "2-EMCI", "3-LMCI", "4-AD")
legend("bottomright", 
       legend = category_names,   # Use category names
       col = colors,              # Use the same colors as in the plot
       lwd = 2)
  
#-AUC-------------------------------------------------------------------------
  
# Micro-average AUC: considers each individual prediction
micro_roc <- multiclass.roc(test_data$diagnosisAD, pred_probs)
micro_auc_rf <- auc(micro_roc)
print(paste("Micro-average AUC - RF:", micro_auc_rf))
  
# Macro-average AUC: average of each class's AUC
macro_auc_rf <- mean(sapply(roc_curves, auc))
print(paste("Macro-average AUC - RF:", macro_auc_rf))
  
#-CONFUSION MATRIX------------------------------------------------------------
  
# Convert predicted probabilities to predicted classes
predicted_classes <- predict(rf_model, newdata = test_data)
actual_classes <- test_data$diagnosisAD
  
# Create a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(actual_classes))
saveRDS(confusion_matrix, file = "~/Capstone_Final/Outputs/confusion_matrix_rf.rds")
print(confusion_matrix)

# Calculate accuracy manually
conf_matrix <- confusion_matrix$table
accuracy_rf <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy - RF:", round(accuracy_rf, 4)))
  
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
print(paste("Macro F1 Score - RF:", macro_f1))
  
# Convert confusion matrix to data frame
heatmap_data <- as.data.frame(as.table(confusion_matrix))
colnames(heatmap_data) <- c("Predicted", "Actual", "Freq")
heatmap_data$Actual <- factor(heatmap_data$Actual, levels = c(1, 2, 3, 4))
heatmap_data$Predicted <- factor(heatmap_data$Predicted, levels = c(4, 3, 2, 1))
  
# Plot heatmap with ggplot2
ggplot(heatmap_data, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +  # Creates the heatmap cells with white borders
  scale_fill_gradient(low = "white", high = "purple") +  # Define color gradient
  geom_text(aes(label = Freq), color = "black") +  # Display cell values
  labs(title = "Confusion Matrix Heatmap - Random Forest", x = "Predicted", y = "Actual") +
  theme_dark()