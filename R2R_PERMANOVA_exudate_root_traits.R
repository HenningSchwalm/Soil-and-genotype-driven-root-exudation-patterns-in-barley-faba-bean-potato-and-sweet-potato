# PERMANOVA separated for biomass data and exudate data


rm(list=ls())
dev.off()

###packages required

library(ggplot2)
library(plyr)
library(MASS)
library(ggfortify)
library(ggthemes)
library(colorspace)
library(grDevices)
library(dunn.test)
library(car)
library(moments)
library(LambertW)
library(ggpubr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(devtools)
library(ggbiplot)
library(pairwiseAdonis)
library(knitr)
library(corrplot)
library(vegan)
library(pairwiseAdonis)
library(openxlsx)
library(performance)
devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis", force = TRUE)



# Load the data
setwd("~/Root2Res/R2R - Results/R_with_Carmen")
setwd ("C:/Users/Henning/Documents/Root2Res/R2R - Results/R_with_Carmen/all_data")
getwd()
df <- read.delim("permanova_exudates_root_traits.txt", sep = "\t")
str(df)

#outlier removal for chapter 2 (Correlation biomass morphology exudation)
df_clean <- df[!df$Sample_ID %in% 
                 c("27. BF_Fairytale","24. AD_Irina","60. BF_Irina","61. AD_Irina",
                   "63. BF_Planet","65. ARV_Fairytale","66. AD_Planet",
                   "68. BF_Laureate","69. BF_Laureate","70. BF_Planet",
                   "71. AD_Laureate","73. BF_Fairytale","74. AD_Fairytale",
                   "75. ARV_Irina",
                   
                   "45. ILB_AD","20. Zoran_AD", "26. Zoran_BF", "32. ILB_AR",
                   
                   "75. Duke of York_ARV", "15. Cara_BF", "50. Innovator_ARV", 
                   "62. Duke of York_BF", "74. Desiree_BF","88. Duke of York_ARV",
                  
                    "2. Minamiyutaka_ARV", "4. Minamiyutaka_BF",
                   "40. Blesbok_BF", "14. Blesbok_ADAS",
                   "24. Blesbok_ARV", "37. Blesbok_BF"),]  

#outlier removal for chapter 1 (only exudation)
df_clean_ex <- df[!df$Sample_ID %in% 
                 c("24. AD_Irina","60. BF_Irina","61. AD_Irina",
          "63. BF_Planet","65. ARV_Fairytale","66. AD_Planet",
          "68. BF_Laureate","69. BF_Laureate","70. BF_Planet",
          "71. AD_Laureate","73. BF_Fairytale","74. AD_Fairytale",
          "75. ARV_Irina",

          "45. ILB_AD",

          "75. Duke of York_ARV",

          "2. Minamiyutaka_ARV", "4. Minamiyutaka_BF",
                "40. Blesbok_BF", "14. Blesbok_ADAS",
                "24. Blesbok_ARV"),]




#subsets
barley_df <- subset(df_clean_ex , Crop == "Barley")
faba_df <- subset(df_clean_ex , Crop == "Faba_bean")
potato_df <- subset(df_clean_ex , Crop == "Potato")
SP_df <- subset(df_clean_ex , Crop == "Sweet_potato")

#remove planet reference plants
faba_df = subset(faba_df, !Genotype == "Planet")
potato_df = subset(potato_df, !Genotype == "Planet")
SP_df = subset(SP_df, !Genotype == "Planet")

df_clean_ex = subset(df_clean_ex, !Genotype == "Planet")

#Plan A : Orientation based on von Haden & Michael (bergmann + only total C)

Barley <- barley_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, 
                                       C_ex_RSA, Norg_ex_RSA, RTD, SRL, RD_mm,)

Faba <- faba_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, 
                           C_ex_RSA, Norg_ex_RSA, RTD, SRL, RD_mm,)

Potato <- potato_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, 
                               C_ex_RSA, Norg_ex_RSA, RTD, SRL, RD_mm,)

SP <- SP_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, 
                       C_ex_RSA, Norg_ex_RSA, RTD, SRL, RD_mm,)



#Plan B : Focus on plant growth vs exudation rate

Barley <- barley_df %>% select(Sample_ID, Genotype, Soil, 
                                       C_ex_RSA, Norg_ex_RSA, Glucose_RSA, CGA_RSA, AA_RSA, unknown_RSA)

Faba <- faba_df %>% select(Sample_ID, Genotype, Soil, 
                           C_ex_RSA, Norg_ex_RSA, Glucose_RSA, CGA_RSA, AA_RSA, unknown_RSA)

Potato <- potato_df %>% select(Sample_ID, Genotype, Soil, 
                               C_ex_RSA, Norg_ex_RSA, Glucose_RSA, CGA_RSA, AA_RSA, unknown_RSA)

SP <- SP_df %>% select(Sample_ID, Genotype, Soil, 
                       C_ex_RSA, Norg_ex_RSA, Glucose_RSA, CGA_RSA, AA_RSA, unknown_RSA)




#Table Paper for Chapter1 "Biomass and Root Morphology" 


Barley <- barley_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, RTD, SRL, RD_mm,)

Faba <- faba_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, RTD, SRL, RD_mm,)

Potato <- potato_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, RTD, SRL, RD_mm,)

SP <- SP_df %>% select(Sample_ID, Genotype, Soil,RDW, SDW, R.S, RTD, SRL, RD_mm,)


#Table Paper for Chapter1 "Exudation Percentages" 


Barley <- barley_df %>% select(Sample_ID, Genotype, Glucose_part, AA_median_part, CGA_part, unknown_part)

Faba <- faba_df %>% selectselect(Sample_ID, Genotype, Glucose_part, AA_median_part, CGA_part, unknown_part)

Potato <- potato_df %>% selectselect(Sample_ID, Genotype, Glucose_part, AA_median_part, CGA_part, unknown_part)

SP <- SP_df %>% selectselect(Sample_ID, Genotype, Glucose_part, AA_median_part, CGA_part, unknown_part)



## Prepare the data
Barley <- na.omit(Barley)
Faba <- na.omit(Faba)
Potato <- na.omit(Potato)
SP <- na.omit(SP)


#calculate all the means for chapter 3 (biomass, winrhizo, exudates)



# Calculate mean, min, and max of C_ex_RSA
mean_C <- mean(df_clean_ex$C_ex_RSA, na.rm = TRUE)
min_C <- min(df_clean_ex$C_ex_RSA, na.rm = TRUE)
max_C <- max(df_clean_ex$C_ex_RSA, na.rm = TRUE)

# Print results
cat("Mean:", mean_C, "\n")
cat("Min:", min_C, "\n")
cat("Max:", max_C, "\n")

# Calculate mean, min, and max of C_ex_RSA
mean_C <- mean(Barley$C_ex_RSA, na.rm = TRUE)
min_C <- min(Barley$C_ex_RSA, na.rm = TRUE)
max_C <- max(Barley$C_ex_RSA, na.rm = TRUE)

# Print results
cat("Mean:", mean_C, "\n")
cat("Min:", min_C, "\n")
cat("Max:", max_C, "\n")

# Calculate mean, min, and max of C_ex_RSA
mean_C <- mean(Faba$C_ex_RSA, na.rm = TRUE)
min_C <- min(Faba$C_ex_RSA, na.rm = TRUE)
max_C <- max(Faba$C_ex_RSA, na.rm = TRUE)

# Print results
cat("Mean:", mean_C, "\n")
cat("Min:", min_C, "\n")
cat("Max:", max_C, "\n")
# Calculate mean, min, and max of C_ex_RSA
mean_C <- mean(Potato$C_ex_RSA, na.rm = TRUE)
min_C <- min(Potato$C_ex_RSA, na.rm = TRUE)
max_C <- max(Potato$C_ex_RSA, na.rm = TRUE)

# Print results
cat("Mean:", mean_C, "\n")
cat("Min:", min_C, "\n")
cat("Max:", max_C, "\n")
# Calculate mean, min, and max of C_ex_RSA
mean_C <- mean(SP$C_ex_RSA, na.rm = TRUE)
min_C <- min(SP$C_ex_RSA, na.rm = TRUE)
max_C <- max(SP$C_ex_RSA, na.rm = TRUE)

# Print results
cat("Mean:", mean_C, "\n")
cat("Min:", min_C, "\n")
cat("Max:", max_C, "\n")

# Define a function to calculate mean and standard error
calculate_summary <- function(data, group_var, variables) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(across(all_of(variables), 
                     list(mean = ~mean(.x, na.rm = TRUE), 
                          se = ~sd(.x, na.rm = TRUE) / sqrt(n())), 
                     .names = "{col}_{fn}"))
}

# List of datasets and their names
datasets <- list(
  Barley = Barley,
  Faba = Faba,
  Potato = Potato,
  SP = SP
)

# Prepare the datasets by removing NA values
datasets <- lapply(datasets, na.omit)

# Initialize an empty list to store results
results <- list()

# Variables to calculate means for
variables_for_means <- c("RDW", "SDW", "R.S", "RTD", "SRL", "RD_mm")

# Loop through each dataset, calculate summary, and store results
for (crop in names(datasets)) {
  data <- datasets[[crop]]
  summary <- calculate_summary(data, "Soil", variables_for_means)
  summary$Crop <- crop  # Add crop name as a column
  results[[crop]] <- summary
}

# Combine all results into one data frame
final_results <- bind_rows(results)

# Save the results to an Excel sheet
write.xlsx(final_results, "Crop_Soil_Summary.xlsx", row.names = FALSE)

# Print a message
cat("Summary saved to 'Crop_Soil_Summary.xlsx'")


#two way anova without date effect
m1 <-  aov(SRL ~  Genotype * Soil , data = Barley)
m1
summary(m1)
TukeyHSD(m1, which = c("Soil", "Genotype"))
# Extract Genotype p-values
tukey_geno <- TukeyHSD(m1, which = "Genotype")$Genotype
pvals <- tukey_geno[, "p adj"]
names(pvals) <- rownames(tukey_geno)
# Compute CLD based on 0.05 threshold
cld <- multcompLetters(pvals, threshold = 0.05)
cld$Letters

# Check normality of residuals using the performance package
check_normality(m1)
#Homogeneity of variance
leveneTest(residuals(m1) ~ Barley$Soil * #two way anova without date effect
m1 <-  aov(C_ex_RSA ~  Genotype * Soil  , data = Barley)
m1
summary(m1)
TukeyHSD(m1, which = c("Soil", "Genotype"))
# Check normality of residuals using the performance package
check_normality(m1)
#Homogeneity of variance
leveneTest(residuals(m1) ~ Barley$Soil * Barley$Genotype)


#Faba
#two way anova without date effect
m2 <-  aov(RD_mm ~  Genotype * Soil , data = Faba)
m2
summary(m2)
TukeyHSD(m2, which = c("Soil", "Genotype"))
tukey_geno <- TukeyHSD(m2, which = "Genotype")$Genotype
pvals <- tukey_geno[, "p adj"]
names(pvals) <- rownames(tukey_geno)
# Compute CLD based on 0.05 threshold
cld <- multcompLetters(pvals, threshold = 0.05)
cld$Letters
# Check normality of residuals using the performance package
check_normality(m2)
#Homogeneity of variance
leveneTest(residuals(m2) ~ Faba$Soil * Faba$Genotype)


#Potato
#two way anova without date effect
m3 <-  aov(RD_mm ~  Genotype * Soil , data = Potato)
m3
summary(m3)
TukeyHSD(m3, which = c("Soil", "Genotype", "Date"))
tukey_geno <- TukeyHSD(m3, which = "Genotype")$Genotype
pvals <- tukey_geno[, "p adj"]
names(pvals) <- rownames(tukey_geno)
# Compute CLD based on 0.05 threshold
cld <- multcompLetters(pvals, threshold = 0.05)
cld$Letters
# Check normality of residuals using the performance package
check_normality(m3)
#Homogeneity of variance
leveneTest(residuals(m3) ~ Potato$Soil * Potato$Genotype)


#Sweet potato
#two way anova without date effect
m4 <-  aov(RD_mm ~  Genotype * Soil  , data = SP)
m4
summary(m4)
TukeyHSD(m4, which = c("Soil", "Genotype"))
emmeans::emmeans(m4, pairwise ~ Genotype | Soil)
tukey_geno <- TukeyHSD(m4, which = "Genotype")$Genotype
pvals <- tukey_geno[, "p adj"]
names(pvals) <- rownames(tukey_geno)
# Compute CLD based on 0.05 threshold
cld <- multcompLetters(pvals, threshold = 0.05)
cld$Letters
# Check normality of residuals using the performance package$Genotype)


         
                              
                              
                              
                              
                          
                              
                              
                              # run PERMANOVA for all results



#BARLEY bray & euclidean
# Extract only numerical data (exudate parameters)
numerical_data_B <- Barley[, c(  "Glucose_RSA", "CGA_RSA", "AA_RSA", "C_ex_RSA", "Norg_ex_RSA", "unknown_RSA"  )]

# Compute Bray-Curtis Distance Matrix
dist_matrix <- vegdist(numerical_data_B, method = "bray")
#vs euclidean
dist_matrix_euc <- dist(numerical_data_B, method = "euclidean")

# Test homogeneity for Soil
betadisper_soil <- betadisper(dist_matrix, Barley$Soil)
anova(betadisper_soil)  # p > 0.05 means assumption is met
plot(betadisper_soil)
# Test homogeneity for Genotype
betadisper_genotype <- betadisper(dist_matrix, Barley$Genotype)
anova(betadisper_genotype)  # p > 0.05 means assumption is met
plot(betadisper_genotype)

#Test homogeneity for Soil euclidean
betadisper_soil_euc <- betadisper(dist_matrix_euc, Barley$Soil)
anova(betadisper_soil_euc)
plot(betadisper_soil_euc)
# Test homogeneity for Genotype euclidean
betadisper_genotype_euc <- betadisper(dist_matrix_euc, Barley$Genotype)
anova(betadisper_genotype_euc)  # p > 0.05 means assumption is met
plot(betadisper_genotype_euc)


#set seed for reproducibility
set.seed(123)
#bray
adonis2(dist_matrix ~ Soil * Genotype, data = Barley, permutations = 999)
#euclidean
adonis2(dist_matrix_euc ~ Soil * Genotype, data = Barley, permutations = 999)



#FABA bray & euclidean
# Extract only numerical data (exudate parameters)
numerical_data_F <- Faba[, c( "Glucose_RSA", "CGA_RSA", "AA_RSA", "C_ex_RSA", "Norg_ex_RSA", "unknown_RSA"  )]

# Compute Bray-Curtis Distance Matrix
dist_matrix <- vegdist(numerical_data_F, method = "bray")
#vs euclidean
dist_matrix_euc <- dist(numerical_data_F, method = "euclidean")

# Test homogeneity for Soil
betadisper_soil <- betadisper(dist_matrix, Faba$Soil)
anova(betadisper_soil)  # p > 0.05 means assumption is met
plot(betadisper_soil)
# Test homogeneity for Genotype
betadisper_genotype <- betadisper(dist_matrix, Faba$Genotype)
anova(betadisper_genotype)  # p > 0.05 means assumption is met
plot(betadisper_genotype)

#Test homogeneity for Soil euclidean
betadisper_soil_euc <- betadisper(dist_matrix_euc, Faba$Soil)
anova(betadisper_soil_euc)
plot(betadisper_soil_euc)
# Test homogeneity for Genotype euclidean
betadisper_genotype_euc <- betadisper(dist_matrix_euc, Faba$Genotype)
anova(betadisper_genotype_euc)  # p > 0.05 means assumption is met
plot(betadisper_genotype_euc)


#set seed for reproducibility
set.seed(123)
#bray
adonis2(dist_matrix ~ Soil * Genotype, data = Faba, permutations = 999)
#euclidean
adonis2(dist_matrix_euc ~ Soil * Genotype, data = Faba, permutations = 999)






#POTATO bray & euclidean
# Extract only numerical data (exudate parameters)
numerical_data_P <- Potato[, c( "Glucose_RSA", "CGA_RSA", "AA_RSA", "C_ex_RSA", "Norg_ex_RSA", "unknown_RSA" )]

# Compute Bray-Curtis Distance Matrix
dist_matrix <- vegdist(numerical_data_P, method = "bray")
#vs euclidean
dist_matrix_euc <- dist(numerical_data_P, method = "euclidean")

# Test homogeneity for Soil
betadisper_soil <- betadisper(dist_matrix, Potato$Soil)
anova(betadisper_soil)  # p > 0.05 means assumption is met
plot(betadisper_soil)
# Test homogeneity for Genotype
betadisper_genotype <- betadisper(dist_matrix, Potato$Genotype)
anova(betadisper_genotype)  # p > 0.05 means assumption is met
plot(betadisper_genotype)

#Test homogeneity for Soil euclidean
betadisper_soil_euc <- betadisper(dist_matrix_euc, Potato$Soil)
anova(betadisper_soil_euc)
plot(betadisper_soil_euc)
# Test homogeneity for Genotype euclidean
betadisper_genotype_euc <- betadisper(dist_matrix_euc, Potato$Genotype)
anova(betadisper_genotype_euc)  # p > 0.05 means assumption is met
plot(betadisper_genotype_euc)


#set seed for reproducibility
set.seed(123)
#bray
adonis2(dist_matrix ~ Soil * Genotype, data = Potato, permutations = 999)
#euclidean
adonis2(dist_matrix_euc ~ Soil * Genotype, data = Potato, permutations = 999)







#SWEET POTATO bray & euclidean
# Extract only numerical data (exudate parameters)
numerical_data_SP <- SP[, c( "Glucose_RSA", "CGA_RSA", "AA_RSA", "C_ex_RSA", "Norg_ex_RSA", "unknown_RSA")]

# Compute Bray-Curtis Distance Matrix
dist_matrix <- vegdist(numerical_data_SP, method = "bray")
#vs euclidean
dist_matrix_euc <- dist(numerical_data_SP, method = "euclidean")

# Test homogeneity for Soil
betadisper_soil <- betadisper(dist_matrix, SP$Soil)
anova(betadisper_soil)  # p > 0.05 means assumption is met
plot(betadisper_soil)
# Test homogeneity for Genotype
betadisper_genotype <- betadisper(dist_matrix, SP$Genotype)
anova(betadisper_genotype)  # p > 0.05 means assumption is met
plot(betadisper_genotype)

#Test homogeneity for Soil euclidean
betadisper_soil_euc <- betadisper(dist_matrix_euc, SP$Soil)
anova(betadisper_soil_euc)
plot(betadisper_soil_euc)
# Test homogeneity for Genotype euclidean
betadisper_genotype_euc <- betadisper(dist_matrix_euc, SP$Genotype)
anova(betadisper_genotype_euc)  # p > 0.05 means assumption is met
plot(betadisper_genotype_euc)



#set seed for reproducibility
set.seed(123)
#bray
adonis2(dist_matrix ~ Soil * Genotype, data = SP, permutations = 999)
#euclidean
adonis2(dist_matrix_euc ~ Soil * Genotype, data = SP, permutations = 999)






################################################################################

# old script for biomass and winrhizo


#careful: always when adding new results to matrix file, excluding certain columns in code to be updated!

#Run PERMANOVA only biomass and root morphology as shown in table paper
#BARLEY
# Split the dataset into categorical and numerical data
categorical_data_B <- Barley[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_B <- Barley[, !(names(Barley) %in% c("Sample_ID", "Genotype", "Soil"))]
exclude_cols <- c(1:5, 8:9, 12:13, 17, 19:50)
numerical_data_B <- Barley[, -exclude_cols]
str(numerical_data_B)
# Ensure the categorical variables are factors
categorical_data_B$Soil <- as.factor(categorical_data_B$Soil)
categorical_data_B$Genotype <- as.factor(categorical_data_B$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_B, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_B$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_B$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_B, permutations=999)



# FABA
# Split the dataset into categorical and numerical data
categorical_data_F <- Faba[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_F <- Faba[, !(names(Faba) %in% c("Sample_ID", "Genotype", "Soil"))]
exclude_cols <- c(1:5, 8:9, 12:13, 18:53)
numerical_data_F <- Faba[, -exclude_cols]
str(numerical_data_F)

# Ensure the categorical variables are factors
categorical_data_F$Soil <- as.factor(categorical_data_F$Soil)
categorical_data_F$Genotype <- as.factor(categorical_data_F$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_F, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_F$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_F$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_F, permutations=999)


#POTATO
# Split the dataset into categorical and numerical data
categorical_data_P <- Potato[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_P <- Potato[, !(names(Potato) %in% c("Sample_ID", "Genotype", "Soil"))]
exclude_cols <- c(1:5, 8:9, 12:13, 18:51)
numerical_data_P <- Potato[, -exclude_cols]
str(numerical_data_P)

# Ensure the categorical variables are factors
categorical_data_P$Soil <- as.factor(categorical_data_P$Soil)
categorical_data_P$Genotype <- as.factor(categorical_data_P$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_P, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_P$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_P$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_P, permutations=999)

#SWEET POTATO
# Split the dataset into categorical and numerical data
categorical_data_SP <- SP[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_SP <- SP[, !(names(SP) %in% c("Sample_ID", "Genotype", "Soil"))]
exclude_cols <- c(1:5, 8:9, 12:13, 18:47)
numerical_data_SP <- SP[, -exclude_cols]
str(numerical_data_SP)
# Ensure the categorical variables are factors
categorical_data_SP$Soil <- as.factor(categorical_data_SP$Soil)
categorical_data_SP$Genotype <- as.factor(categorical_data_SP$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_SP, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_SP$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_SP$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_SP, permutations=999)



###############################################################################

#ROOT EXUDATES

#Run PERMANOVA only exudates normalised by RDW
#BARLEY
# Split the dataset into categorical and numerical data
categorical_data_B <- Barley[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_B <- Barley[, !(names(Barley) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(18:19, 35, 38, 41)
numerical_data_B <- Barley[, select_cols]
str(numerical_data_B)
# Ensure the categorical variables are factors
categorical_data_B$Soil <- as.factor(categorical_data_B$Soil)
categorical_data_B$Genotype <- as.factor(categorical_data_B$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_B, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_B$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_B$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_B, permutations=999)



# FABA
# Split the dataset into categorical and numerical data
categorical_data_F <- Faba[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_F <- Faba[, !(names(Faba) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(18:19, 38, 41, 44)
numerical_data_F <- Faba[, select_cols]
str(numerical_data_F)

# Ensure the categorical variables are factors
categorical_data_F$Soil <- as.factor(categorical_data_F$Soil)
categorical_data_F$Genotype <- as.factor(categorical_data_F$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_F, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_F$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_F$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_F, permutations=999)


#POTATO
# Split the dataset into categorical and numerical data
categorical_data_P <- Potato[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_P <- Potato[, !(names(Potato) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(18:19, 39, 42, 45)
numerical_data_P <- Potato[, select_cols]

# Ensure the categorical variables are factors
categorical_data_P$Soil <- as.factor(categorical_data_P$Soil)
categorical_data_P$Genotype <- as.factor(categorical_data_P$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_P, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_P$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_P$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_P, permutations=999)

#SWEET POTATO
# Split the dataset into categorical and numerical data
categorical_data_SP <- SP[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_SP <- SP[, !(names(SP) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(31:32, 41, 35, 38)
numerical_data_SP <- SP[, select_cols]
str(numerical_data_SP)
# Ensure the categorical variables are factors
categorical_data_SP$Soil <- as.factor(categorical_data_SP$Soil)
categorical_data_SP$Genotype <- as.factor(categorical_data_SP$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_SP, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_SP$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_SP$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_SP, permutations=999)




#ROOT EXUDATES

#Run PERMANOVA only exudates normalised by RSA
#BARLEY
# Split the dataset into categorical and numerical data
categorical_data_B <- Barley[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_B <- Barley[, !(names(Barley) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(20:21, 36, 39, 42)
numerical_data_B <- Barley[, select_cols]
str(numerical_data_B)
# Ensure the categorical variables are factors
categorical_data_B$Soil <- as.factor(categorical_data_B$Soil)
categorical_data_B$Genotype <- as.factor(categorical_data_B$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_B, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_B$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_B$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_B, permutations=999)



# FABA
# Split the dataset into categorical and numerical data
categorical_data_F <- Faba[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_F <- Faba[, !(names(Faba) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(20:21, 39, 42, 45)
numerical_data_F <- Faba[, select_cols]
str(numerical_data_F)

# Ensure the categorical variables are factors
categorical_data_F$Soil <- as.factor(categorical_data_F$Soil)
categorical_data_F$Genotype <- as.factor(categorical_data_F$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_F, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_F$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_F$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_F, permutations=999)


#POTATO
# Split the dataset into categorical and numerical data
categorical_data_P <- Potato[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_P <- Potato[, !(names(Potato) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(20:21, 40, 43, 46)
numerical_data_P <- Potato[, select_cols]

# Ensure the categorical variables are factors
categorical_data_P$Soil <- as.factor(categorical_data_P$Soil)
categorical_data_P$Genotype <- as.factor(categorical_data_P$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_P, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_P$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_P$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_P, permutations=999)

#SWEET POTATO
# Split the dataset into categorical and numerical data
categorical_data_SP <- SP[, c("Sample_ID", "Genotype", "Soil")]
numerical_data_SP <- SP[, !(names(SP) %in% c("Sample_ID", "Genotype", "Soil"))]
select_cols <- c(33:34, 36, 39, 42)
numerical_data_SP <- SP[, select_cols]
str(numerical_data_SP)
# Ensure the categorical variables are factors
categorical_data_SP$Soil <- as.factor(categorical_data_SP$Soil)
categorical_data_SP$Genotype <- as.factor(categorical_data_SP$Genotype)

#distance matrix
dist_matrix <- vegdist(numerical_data_SP, method = "bray")

# Check for homogeneity of dispersion for Soil
betadisper_soil <- betadisper(dist_matrix, categorical_data_SP$Soil)
anova(betadisper_soil)
plot(betadisper_soil)

# Check for homogeneity of dispersion for Genotype
betadisper_genotype <- betadisper(dist_matrix, categorical_data_SP$Genotype)
anova(betadisper_genotype)
plot(betadisper_genotype)

# PERMANOVA 
adonis2(dist_matrix ~ Soil*Genotype, data=categorical_data_SP, permutations=999)

