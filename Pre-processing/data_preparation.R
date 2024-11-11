# Import the necessary libraries
library(tidyverse)
library(haven)
library(dplyr)
library(fastDummies)
library(nnet)
library(caret)
library(classpackage)
library(pROC)
library(ggplot2)
library(mice)
library(DMwR2)
library(e1071)        # Needed for SVM model
library(randomForest) # Needed for RF model
library(reshape2)
library(xtable)
library(gridExtra)
library(cowplot)  

# Set working directory
setwd("~/Capstone_Final/")
getwd()

# Read the ADNI data file      
Data_D1D2 <- read_csv("TADPOLE_D1_D2_2.csv")

# Extract Training Data to include subjects at baseline that were NC, EMCI, LMCI, or AD
Data_D1D2 <- Data_D1D2 %>%
  filter(VISCODE == 'bl',
         DX_bl != "SMC") %>%
    select(RID, DX_bl, CDRSB, ADAS11, MMSE, RAVLT_immediate, AGE, PTGENDER) %>%
    na.omit(RID) # Omit subjects that do not have IDs

# NAs checked and none exist
Data_D1D2 <- data_frame(Data_D1D2)

# Scale continuous Predictors
Data_D1D2 <- Data_D1D2 %>%
  mutate(
    RAVLT_immediate = scale(RAVLT_immediate),
    CDRSB = scale(CDRSB),
    ADAS11 = scale(ADAS11),
    MMSE = scale(MMSE),
    RAVLT_immediate = scale(RAVLT_immediate))

# Save data
write.csv(Data_D1D2, "~/Capstone_Final/Data_D1D2.csv", row.names = TRUE)


