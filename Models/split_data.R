# Set working directory
setwd("~/Capstone_Final/")
getwd()

# Retrieve data
Data_D1D2 <- read_csv("Data_D1D2.csv")

# Create a partition for training data 70/30 split
set.seed(123)
train_index <- createDataPartition(Data_D1D2$DX_bl, p = 0.7, list = FALSE)  # 70% for training

# Split the dataset into training and test sets
Training_Data_D1D2 <- Data_D1D2[train_index, ]
Test_Data_D1D2 <- Data_D1D2[-train_index, ]

# Check the dimensions of the training and test sets
# cat("Training set dimensions:", dim(Training_Data_D1D2), "\n")
# cat("Test set dimensions:", dim(Test_Data_D1D2), "\n")

# Create a boxplot of the total dataset that shows diagnosis by age and gender
ggplot(Data_D1D2, aes(x = DX_bl, y = AGE, fill = PTGENDER)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +  # Apply dodge to counts to avoid overlap
  stat_summary(fun.data = function(x) {
    data.frame(
      y = median(x)+2,  # Place the count at the median position or adjust to fit within the box
      label = length(x)
    )
  }, geom = "text", size = 3, color = "black", 
  position = position_dodge(width = 0.8)) +  # Apply dodge to counts to avoid overlap
  labs(title = "Age by Diagnosis and Gender - Training Data",
       x = "Diagnosis",
       y = "Age") +
  scale_fill_manual(values = c("lightblue", "pink")) +  # Adjust colors as needed
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "italic", color = "darkblue", size = 14),
    axis.title.y = element_text(face = "italic", color = "darkblue", size = 14),
    axis.text = element_text(color = "blue", size = 10),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightgrey", color = "grey70")
  )

# Create an indicator variable of baseline diagnosis for both training and test data
Training_Data_D1D2 <- Training_Data_D1D2 %>%
  mutate(diagnosisAD = case_when(
        DX_bl == "CN" ~ 1,
        DX_bl == "EMCI" ~ 2,
        DX_bl == "LMCI" ~ 3,
        DX_bl == "AD" ~ 4 ))
  Test_Data_D1D2 <- Test_Data_D1D2 %>%
  mutate(diagnosisAD = case_when(
        DX_bl == "CN" ~ 1,
        DX_bl == "EMCI" ~ 2,
        DX_bl == "LMCI" ~ 3,
        DX_bl == "AD" ~ 4 ))

# Plot Age Distribution by Diagnosis
combined_Data_D1D2 <- rbind(Training_Data_D1D2, Test_Data_D1D2) # Combine training and test data
diagnosis_labels <- c("NC", "EMCI", "LMCI", "AD") # Define custom labels for the diagnosis categories
age_plot <- ggplot(combined_Data_D1D2, aes(x = AGE, fill = factor(diagnosisAD))) +
  geom_histogram(position = "stack", bins = 30, alpha = 0.7) +
  labs(title = "Age Distribution by Diagnosis", x = "AGE", y = "Count", fill = "Diagnosis") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"), 
  labels = diagnosis_labels) +
  theme_dark() 
# Plot Gender Distribution by Diagnosis
gender_plot <- ggplot(combined_Data_D1D2, aes(x = PTGENDER, fill = factor(diagnosisAD))) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Gender Distribution by Diagnosis", x = "Gender", y = "Count", fill = "Diagnosis") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"), 
  labels = diagnosis_labels) +
  theme_dark() 
# Arrange the two plots side-by-side
grid.arrange(age_plot, gender_plot, ncol = 2)

# Calculate class weights for balancing data during model execution
class_freq <- table(Training_Data_D1D2$diagnosisAD)
class_weights <- 1 / class_freq
class_weights <- class_weights / sum(class_weights) #normalize

# Combine into a list
data_list <- list(CDRSB = Training_Data_D1D2$CDRSB, ADAS11 = Training_Data_D1D2$ADAS11, MMSE = Training_Data_D1D2$MMSE, RAVLT = Training_Data_D1D2$RAVLT_immediate)

# Boxplot with colors
boxplot(data_list, 
        col = c("skyblue", "salmon", "lightgreen", "gold"),
        main = "Outliers Not Handled",
        xlab = "Cognitive Test",
        ylab = "Scores")

# Identify outliers as points that are 3 standard deviations away from the mean
outliersCDRSB <- abs(Training_Data_D1D2$CDRSB) > 3
outliersADAS11 <- abs(Training_Data_D1D2$ADAS11) > 3
outliersMMSE  <- abs(Training_Data_D1D2$MMSE) > 3
outliersRAVLT  <- abs(Training_Data_D1D2$RAVLT_immediate) > 3

# Handle outliers - Replace outliers that exceed threshold with the median value
Training_Data_D1D2$CDRSB[outliersCDRSB] <- median(Training_Data_D1D2$CDRSB, na.rm = TRUE)
Training_Data_D1D2$ADAS11[outliersADAS11] <- median(Training_Data_D1D2$ADAS11, na.rm = TRUE)
Training_Data_D1D2$MMSE[outliersMMSE] <- median(Training_Data_D1D2$MMSE, na.rm = TRUE)
Training_Data_D1D2$RAVLT_immediate[outliersRAVLT] <- median(Training_Data_D1D2$RAVLT_immediate, na.rm = TRUE)

# Combine into a list
data_list <- list(CDRSB = Training_Data_D1D2$CDRSB, ADAS11 = Training_Data_D1D2$ADAS11, MMSE = Training_Data_D1D2$MMSE, RAVLT = Training_Data_D1D2$RAVLT_immediate)

# Boxplot with colors
boxplot(data_list,
        col = c("skyblue", "salmon", "lightgreen", "gold"),
        main = "Outliers Handled",
        xlab = "Cognitive Test",
        ylab = "Scores")

# Write training and test data files
write.csv(Training_Data_D1D2, "training_data.csv", row.names = TRUE)
write.csv(Test_Data_D1D2, "test_data.csv", row.names = TRUE)

