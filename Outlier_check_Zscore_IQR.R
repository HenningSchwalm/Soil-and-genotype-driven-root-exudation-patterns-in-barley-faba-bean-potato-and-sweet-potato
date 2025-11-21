rm(list=ls())
dev.off()

# Load necessary libraries
library(dplyr)        # For data manipulation
library(ggplot2)      # For visualization
library(car)          # For Mahalanobis distance
library(outliers)     # For Grubbs test
library(MASS)         # For robust covariance
library(outliers)
library(openxlsx)     # For saving to Excel

# Load the data
setwd()
getwd()
df <- read.delim()
str(df)

# Function to detect outliers in a single variable
detect_outliers <- function(data, variable) {
  outlier_summary <- data.frame(Variable = character(), Method = character(), Outlier_Index = integer(), Value = numeric(), Sample_ID = character())
  
  # Z-Score Method
  z_scores <- scale(data[[variable]])
  z_outliers <- which(abs(z_scores) > 3)
  if (length(z_outliers) > 0) {
    outlier_summary <- rbind(outlier_summary, data.frame(Variable = variable, Method = "Z-Score", Outlier_Index = z_outliers, Value = data[[variable]][z_outliers], Sample_ID = data$Sample_ID[z_outliers]))
  }
  
  # IQR Method
  Q1 <- quantile(data[[variable]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[variable]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  iqr_outliers <- which(data[[variable]] < (Q1 - 1.5 * IQR) | data[[variable]] > (Q3 + 1.5 * IQR))
  if (length(iqr_outliers) > 0) {
    outlier_summary <- rbind(outlier_summary, data.frame(Variable = variable, Method = "IQR", Outlier_Index = iqr_outliers, Value = data[[variable]][iqr_outliers], Sample_ID = data$Sample_ID[iqr_outliers]))
  }
  
  return(outlier_summary)
}

# Subset the data by crop
crop_groups <- unique(df$Crop)  # Get the unique crop groups (e.g., Barley, Faba Bean)

# Apply outlier detection for each crop and each variable
all_outliers <- do.call(rbind, lapply(crop_groups, function(crop) {
  df_crop <- df[df$Crop == crop, ]  # Subset data by crop
  
  # Apply outlier detection for all numeric variables
  outliers_for_crop <- do.call(rbind, lapply(names(df_crop)[sapply(df_crop, is.numeric)], function(var) {
    detect_outliers(df_crop, var)
  }))
  
  # Add crop information to the summary
  outliers_for_crop$Crop <- crop
  
  return(outliers_for_crop)
}))

# Filter to keep only outliers that appear in both Z-Score and IQR methods
# We'll do this by identifying rows that have the same Outlier_Index across both methods
outliers_zscore <- all_outliers[all_outliers$Method == "Z-Score", ]
outliers_iqr <- all_outliers[all_outliers$Method == "IQR", ]

# Merge the two datasets to find common outliers (i.e., same Outlier_Index)
common_outliers <- merge(outliers_zscore, outliers_iqr, by = c("Sample_ID", "Variable", "Outlier_Index"))

# Save the summary of common outliers for each crop and variable to a CSV file
write.csv(common_outliers, "common_outliers_by_crop_and_variable.csv", row.names = FALSE)

# Save the summary of common outliers for each crop and variable to an Excel file
write.xlsx(common_outliers, "common_outliers_by_crop_and_variable.xlsx", rowNames = FALSE)

# Optionally, visualize the common outliers for a specific variable (e.g., "C_ex_RSA" for Barley)
ggplot(df[df$Crop == "Faba_bean", ], aes(x = factor(Sample_ID), y = RTD)) +
  geom_point(color = "blue") +
  geom_point(data = common_outliers[common_outliers$Crop == "Barley" & 
                                      common_outliers$Variable == "respective trait", ], 
             aes(x = factor(Sample_ID), y = Value.x), color = "red", size = 3) +  
  labs(title = "Outlier Detection", x = "Sample ID", y = "R:S") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability


