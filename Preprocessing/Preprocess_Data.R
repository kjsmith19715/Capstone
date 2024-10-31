
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
library(reshape2)
library(caret)
library(mice)
library(DMwR2)


# Set working directory
setwd("~/Capstone/")
getwd()

# Read the ADNI data file      
Data_D1D2 <- read_csv("TADPOLE_D1_D2_2.csv")

# Rename columns
Data_D1D2 <- Data_D1D2 %>% rename(
  ABETA = ABETA_UPENNBIOMK9_04_19_17,
  TAU = TAU_UPENNBIOMK9_04_19_17,
  PTAU = PTAU_UPENNBIOMK9_04_19_17)



# Extract Training Data to include subjects at baseline that were diagnosed with MCI or AD
Data_D1D2 <- Data_D1D2 %>%
  filter(VISCODE == 'bl',
         DX_bl == 'AD'| DX_bl == 'CN') %>%
  select(RID, VISCODE, DX_bl, DX, DXCHANGE, CDRSB, ADAS11, MMSE, RAVLT_immediate, APOE4, ABETA, TAU, PTAU, Hippocampus, WholeBrain, Entorhinal, Ventricles, Fusiform) %>%
  # select(RID, VISCODE, DX_bl, DX, DXCHANGE, CDRSB, ADAS11, MMSE, RAVLT_immediate) %>%
  na.omit()

# PTAU is a char datatype in the source file, convert to numeric
Data_D1D2 <- Data_D1D2 %>%
  mutate(
    PTAU = as.numeric(PTAU))
view(Data_D1D2$PTAU)

# Scale continuous Predictors
Data_D1D2 <- Data_D1D2 %>%
  mutate(
    ABETA = scale(ABETA),
    PTAU = scale(PTAU),
    TAU = scale(TAU),
    Hippocampus = scale(Hippocampus),
    WholeBrain = scale(WholeBrain),
    Entorhinal = scale(Entorhinal),
    Ventricles = scale(Ventricles),
    Fusiform = scale(Fusiform),
    RAVLT_immediate = scale(RAVLT_immediate),
    CDRSB = scale(CDRSB),
    ADAS11 = scale(ADAS11),
    MMSE = scale(MMSE),
    RAVLT_immediate = scale(RAVLT_immediate))

# Save data
write.csv(Data_D1D2, "Data_D1D2.csv", row.names = TRUE)

