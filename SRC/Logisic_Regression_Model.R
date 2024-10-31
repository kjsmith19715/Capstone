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
anova(reduced, m1, test = "LRT")

car::Anova(m1, type = 3)
round(confint(m1), 4)

# Generate predicted probabilities using the test data set
test_data$predicted_probability <- predict(m1, newdata = test_data, type = "response")
view(test_data)

# Create ROC curve
roc_curve <- roc(test_data$diagnosisAD, test_data$predicted_probability)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve - CSF Biomarkers", col = "blue")

# Print AUC
print(paste("AUC - CSF Biomarkers:", auc(roc_curve)))

# Convert predicted probabilities to predicted classes
predicted_classes <- ifelse(test_data$predicted_probability > .2, 1, 0)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$diagnosisAD))
print(confusion_matrix)

# Plot the confusion matrix
heatmap_data <- as.data.frame(confusion_matrix$table)
colnames(heatmap_data) <- c("Predicted", "Actual", "Freq")
ggplot(heatmap_data, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Confusion Matrix Heatmap")
