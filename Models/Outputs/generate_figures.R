
#-------------------------LOAD DATA ---------------------------------------------------------
# Load the confusion matrix from the .rds file for the Logistic Regression Model
confusion_matrix_glm <- readRDS("confusion_matrix_glm.rds")
roc_curve_glm <- readRDS("roc_curve_glm.RDS")
confusion_matrix_svm <- readRDS("confusion_matrix_svm.rds")
roc_curve_svm <- readRDS("roc_curve_svm.RDS")


#-------------------------DEFINE FUNCTIONS ---------------------------------------------------------
# Define a function to generate a confusion matrix heatmap
plot_confusion_heatmap <- function(confusion_matrix, title = "Confusion Matrix Heatmap", low = "white", high = "blue") {
  heatmap_data <- as.data.frame(confusion_matrix$table)
  colnames(heatmap_data) <- c("Predicted", "Actual", "Freq")
  heatmap_data$Actual <- factor(heatmap_data$Actual, levels = c(0, 1))
  heatmap_data$Predicted <- factor(heatmap_data$Predicted, levels = c(1, 0))
  ggplot(heatmap_data, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient(low = low, high = high) +
    geom_text(aes(label = Freq), vjust = 1) +
    labs(title = title)
  }
  
  #-------------------------GENERATE PLOTS FOR LOGISITC REGRESSION MODEL------------------------------
  
  color <- "blue"
  
  # Generate heatmap of confusion matrix for Logistic Regression Model
  plot_confusion_heatmap(confusion_matrix_glm, title = "Confusion Matrix Heatmap - Logistic Regression Model", "white", color)
  Sys.sleep(1)
  
  # Plot ROC curve
  plot(roc_curve_glm, main = "ROC Curve - Logistic Regression Model", col = color, lwd = 2)
  
  #-------------------------GENERATE PLOTS FOR SVM------------------------------
  
  color <- "green"
  
  # Generate heatmap of confusion matrix for Logistic Regression Model
  plot_confusion_heatmap(confusion_matrix_svm, title = "Confusion Matrix Heatmap - SVM", "white", color)
  Sys.sleep(1)
  
  # Plot ROC curve
  plot(roc_curve_svm, main = "ROC Curve - SVM", col = color, lwd = 2)

