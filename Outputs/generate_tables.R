# Load the data from the Logistic Regression Model
anova_glm_df <- readRDS("anova_df.rds")
read_csv("auc_output_glm.csv")
confusion_matrix_glm <- readRDS("confusion_matrix_glm.rds")

# Prepare LaTeX table function
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
  write(latex_table, file = paste("confusion_matrix_", title, ".tex", sep = ""))
        
}

# generate confusion matrix statstics function
generate_confusion_matrix_stats <- function(cm, title = "confusion_matrix_stats"){
    # Extract overall statistics
    accuracy <- confusion_matrix$overall["Accuracy"]
    kappa <- confusion_matrix$overall["Kappa"]
  
    # Extract class-level statistics
    sensitivity <- confusion_matrix$byClass["Sensitivity"]
    specificity <- confusion_matrix$byClass["Specificity"]
    precision <- confusion_matrix$byClass["Pos Pred Value"] # also called PPV
    recall <- confusion_matrix$byClass["Sensitivity"] # recall is the same as sensitivity
    f1_score <- 2 * (precision * recall) / (precision + recall)

  # Format the statistics in LaTeX
    latex_stats <- paste(
    "\\begin{table}[h]",
    "\\centering",
    "\\begin{tabular}{l|c}",
    "Metric & Value \\\\ \\hline",
    paste("Accuracy &", round(accuracy, 3), "\\\\"),
    paste("Kappa &", round(kappa, 3), "\\\\"),
    paste("Sensitivity &", round(sensitivity, 3), "\\\\"),
    paste("Specificity &", round(specificity, 3), "\\\\"),
    paste("Precision &", round(precision, 3), "\\\\"),
    paste("F1 Score &", round(f1_score, 3), "\\\\"),
    "\\end{tabular}",
    "\\caption{Confusion Matrix Statistics - Logistic Regression Model}",
    "\\end{table}",
    sep = "\n"
    )

# Write to a .tex file
    Sys.sleep(1)
    write(latex_stats, file = paste("confusion_matrix_statistics_", title, ".tex", sep = ""))
}

#---------LOGISITC REGRESSION ANOVA TABLE---------------------

# Format the ANOVA output into LaTeX
latex_table <- paste(
  "\\begin{table}[h]",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "Model & Resid. Df & Df & Deviance & Pr(>Chi) \\\\ \\hline",
  paste("Null Model &", anova_glm_df$Res.Df[1], "& - &", round(anova_glm_df$Deviance[1], 3), "& - \\\\"),
  paste("Full Model &", anova_glm_df$Res.Df[2], "&", anova_glm_df$Df[2], "&", round(anova_glm_df$Deviance[2], 3), "&", ifelse(is.na(anova_glm_df$`Pr(>Chi)`[2]), "-", round(anova_glm_df$`Pr(>Chi)`[2], 3)), "\\\\"),
  "\\end{tabular}",
  "\\caption{Likelihood Ratio Test for Model Significance}",
  "\\end{table}",
  sep = "\n"
)

# Write to a .tex file
write(latex_table, file = "anova_glm_comparison.tex")

#---------LOGISITC REGRESSION CONFUSION MATRIX---------------------

# Generate confusion matrix for Logistic Regression Model
Sys.sleep(1)
generate_confusion_matrix(confusion_matrix_glm, title = "glm")
Sys.sleep(1)
generate_confusion_matrix_stats(confusion_matrix_glm, title = "glm")

