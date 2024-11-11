# Load the data
confusion_matrix_multinom <- readRDS("~/Capstone_Final/Outputs/confusion_matrix_multinom.rds")
confusion_matrix_svm <- readRDS("~/Capstone_Final/Outputs/confusion_matrix_svm.rds")
confusion_matrix_rf <- readRDS("~/Capstone_Final/Outputs/confusion_matrix_rf.rds")

# LaTeX table function
generate_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  conf_matrix_table <- as.table(confusion_matrix)
  latex_table <- paste(
  "\\begin{table}[h]",
  "\\centering",
  "\\begin{tabular}{l|cc}",
  " & Predicted AD & Predicted Other \\\\ \\hline",
  paste("Actual AD &", conf_matrix_table[1, 1], "&", conf_matrix_table[1, 2], "\\\\"),
  paste("Actual Other &", conf_matrix_table[2, 1], "&", conf_matrix_table[2, 2], "\\\\"),
  "\\end{tabular}",
  "\\caption{Title}",
  "\\end{table}",
  sep = "\n"
 )
  # Write to a .tex file
  write(latex_table, file = paste("~/Capstone_Final/Outputs/confusion_matrix_", title, ".tex", sep = ""))
}

# generate confusion matrix statistics function
generate_confusion_matrix_stats <- function(confusion_matrix, title = "confusion_matrix_stats"){
  # Extract overall statistics
  accuracy <- confusion_matrix$overall["Accuracy"]
  kappa <- confusion_matrix$overall["Kappa"]
  # Extract class-level statistics for each class
  sensitivity <- confusion_matrix$byClass[, "Sensitivity"]
  specificity <- confusion_matrix$byClass[, "Specificity"]
  precision <- confusion_matrix$byClass[, "Pos Pred Value"] # Pos Pred Value is PPV
  recall <- sensitivity # Recall is equivalent to sensitivity
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Prepare LaTeX code for a table with class-level metrics
  latex_stats <- paste(
    "\\begin{table}[h]",
    "\\centering",
    "\\begin{tabular}{l|c|c|c|c}",
    "Metric & Class 1 & Class 2 & Class 3 & Class 4 \\\\ \\hline",
    paste("Sensitivity &", paste(round(sensitivity, 3), collapse = " & "), "\\\\"),
    paste("Specificity &", paste(round(specificity, 3), collapse = " & "), "\\\\"),
    paste("Precision &", paste(round(precision, 3), collapse = " & "), "\\\\"),
    paste("F1 Score &", paste(round(f1_score, 3), collapse = " & "), "\\\\"),
    "\\end{tabular}",
    paste("\\caption{Class-wise Confusion Matrix Statistics -", title, "}"),
    "\\end{table}",
    sep = "\n"
    )
  
  # Prepare LaTeX code for a table with overall statistics
  latex_overall_stats <- paste(
    "\\begin{table}[h]",
    "\\centering",
    "\\begin{tabular}{l|c}",
    "Metric & Value \\\\ \\hline",
    paste("Accuracy &", round(accuracy, 3), "\\\\"),
    paste("Kappa &", round(kappa, 3), "\\\\"),
    "\\end{tabular}",
    paste("\\caption{Overall Confusion Matrix Statistics -", title, "}"),
    "\\end{table}",
    sep = "\n"
    )
  
  # Combine both tables into one LaTeX document string
  latex_document <- paste(
    latex_overall_stats,
    latex_stats,
    sep = "\n\n"
    )
    
    # Write to a .tex file
    Sys.sleep(1)
    write(latex_document, file = paste("~/Capstone_Final/Outputs/confusion_matrix_statistics_", title, ".tex", sep = ""))
}

#---------LOGISITC REGRESSION CONFUSION MATRIX---------------------

# Generate confusion matrix for Logistic Regression Model
Sys.sleep(1)
generate_confusion_matrix(confusion_matrix_multinom, title = "multinom")
Sys.sleep(1)
generate_confusion_matrix_stats(confusion_matrix_multinom, title = "multinom")


#---------SVM CONFUSION MATRIX---------------------

# Generate confusion matrix for SVM
Sys.sleep(1)
generate_confusion_matrix(confusion_matrix_svm, title = "svm")
Sys.sleep(1)
generate_confusion_matrix_stats(confusion_matrix_svm, title = "svm")

#---------SVM CONFUSION MATRIX---------------------

# Generate confusion matrix for RF
Sys.sleep(1)
generate_confusion_matrix(confusion_matrix_rf, title = "rf")
Sys.sleep(1)
generate_confusion_matrix_stats(confusion_matrix_rf, title = "rf")

