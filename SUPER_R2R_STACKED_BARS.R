##Root2res: WP2 Controlled conditions 
######################################



rm(list=ls())
dev.off()

###packages required
library(stringr)
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
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(patchwork)
library(performance)
library(writexl)

##load all the R2R WP2 data

#Barley
setwd()
setwd ()
getwd()
barley_biomass <- read.delim("Barley_biomass.txt", sep = "\t")
head(barley_biomass)

#Faba
setwd ()
getwd()
Faba_biomass <- read.delim("Faba_biomass.txt", sep = "\t")
Faba_biomass_no_planet = subset(Faba_biomass, !Genotype == "Planet")

#Potato
setwd ()
getwd()
Potato_biomass <- read.delim("Potato_biomass.txt", sep = "\t")
Potato_biomass_no_planet = subset(Potato_biomass, !Genotype == "Planet")

#Sweet Potato 
setwd ()
getwd()
SP_biomass <- read.delim("SP_biomass.txt", sep = "\t")
SP_biomass_no_planet = subset(SP_biomass, !Genotype == "Planet")


#clean all the data (affected samples and outliers)

#outlier testing
# based on z-score and IQR test + manually investigation of outliers, the following
# outliers were detected and removed from this analysis




#barley: lost C due to wrong storage in fridge instead in freezer (ID 60 - 75)
# BF_Fairytale BF_planet , BF_Laureate only , AD_Irina 3 remaining replicates, 
barley_biomass_ex = barley_biomass[!barley_biomass$Sample_ID %in% 
                                     c("24. AD_Irina","60. BF_Irina","61. AD_Irina",
                                       "63. BF_Planet","65. ARV_Fairytale","66. AD_Planet",
                                       "68. BF_Laureate","69. BF_Laureate","70. BF_Planet",
                                       "71. AD_Laureate","73. BF_Fairytale","74. AD_Fairytale",
                                       "75. ARV_Irina"),]

#faba: remove strong outlier (45.ILB due to low RSSR!)
Faba_biomass_ex = Faba_biomass_no_planet[!Faba_biomass_no_planet$Sample_ID %in% c("45. ILB_AD"),]

#potato: remove outlier 75. Duke of York_ARV
Potato_biomass_ex = Potato_biomass_no_planet[!Potato_biomass_no_planet$Sample_ID %in% c("75. Duke of York_ARV"),]

#SP: 2. 4. nitrogen (and AA) contaminated, 40. glucose contaminated, 14. and 24 C_ex affected (RSSR) 
# never more than 1 replicate out of 5 affected
SP_biomass_ex = SP_biomass_no_planet[!SP_biomass_no_planet$Sample_ID %in%
                                       c("2. Minamiyutaka_ARV", "4. Minamiyutaka_BF",
                                         "40. Blesbok_BF", "14. Blesbok_ADAS",
                                         "24. Blesbok_ARV"),]


str(barley_biomass_ex)
str(Faba_biomass_ex)
str(Potato_biomass_ex)
str(SP_biomass_ex)


# Generating Means and SE in Excel  Output file

# BARLEY 
# Function to calculate standard error
se <- function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
# List of variables to summarize
variables <- c("C_ex_RSA", "Glucose_C_RSA", "AA_C_RSA", "CGA_C_RSA", "unknown_C_RSA")
# Function to compute mean and SE for a given grouping variable
compute_summary <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(across(all_of(variables), 
                     list(mean = ~mean(.x, na.rm = TRUE), 
                          se = ~se(.x)), 
                     .names = "{.col}_{.fn}"))
}
# Compute summaries
soil_summary <- compute_summary(barley_biomass_ex, "Soil")
genotype_summary <- compute_summary(barley_biomass_ex, "Genotype")

# Save to Excel
write_xlsx(list(
  Soil_Summary = soil_summary,
  Genotype_Summary = genotype_summary
), "stacked_ex_mean_summary.xlsx")

# FABA 
# List of variables to summarize
variables_F <- c("C_ex_RSA", "Glucose_C_RSA_anthrone", "AA_C_RSA", "CGA_C_RSA", "unknown_C_RSA")
# Function to compute mean and SE for a given grouping variable
compute_summary_F <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(across(all_of(variables_F), 
                     list(mean = ~mean(.x, na.rm = TRUE), 
                          se = ~se(.x)), 
                     .names = "{.col}_{.fn}"))
}
# Compute summaries
soil_summary_F <- compute_summary_F(Faba_biomass_ex, "Soil")
genotype_summary_F <- compute_summary_F(Faba_biomass_ex, "Genotype")

# Save to Excel
write_xlsx(list(
  Soil_Summary = soil_summary_F,
  Genotype_Summary = genotype_summary_F
), "stacked_ex_mean_summary_f.xlsx")

# POTATO 
# Compute summaries
soil_summary_P <- compute_summary(Potato_biomass_ex, "Soil")
genotype_summary_P <- compute_summary(Potato_biomass_ex, "Genotype")

# Save to Excel
write_xlsx(list(
  Soil_Summary = soil_summary_P,
  Genotype_Summary = genotype_summary_P
), "stacked_ex_mean_summary_p.xlsx")


# SWEET POTATO 
# List of variables to summarize
variables_SP <- c("C_ex_RSA", "Glucose_C_RSA_AS", "AA_C_RSA", "CGA_C_RSA", "unknown_C_RSA")
# Function to compute mean and SE for a given grouping variable
compute_summary_SP <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(across(all_of(variables_SP), 
                     list(mean = ~mean(.x, na.rm = TRUE), 
                          se = ~se(.x)), 
                     .names = "{.col}_{.fn}"))
}
# Compute summaries
soil_summary_SP <- compute_summary_SP(SP_biomass_ex, "Soil")
genotype_summary_SP <- compute_summary_SP(SP_biomass_ex, "Genotype")

# Save to Excel
write_xlsx(list(
  Soil_Summary = soil_summary_SP,
  Genotype_Summary = genotype_summary_SP
), "stacked_ex_mean_summary_sp.xlsx")


#Statistics


#Barley
#two way anova without date effect
m1 <-  aov(Norg_ex_RSA ~  Genotype * Soil  , data = barley_biomass_ex)
m1
summary(m1)
TukeyHSD(m1, which = c("Soil", "Genotype"))
# Check normality of residuals using the performance package
check_normality(m1)
#Homogeneity of variance
leveneTest(residuals(m1) ~ barley_biomass_ex$Soil * barley_biomass_ex$Genotype)


#Faba
#two way anova without date effect
m2 <-  aov(Norg_ex_RSA ~  Genotype * Soil , data = Faba_biomass_ex)
m2
summary(m2)
TukeyHSD(m2, which = c("Soil", "Genotype"))
# Check normality of residuals using the performance package
check_normality(m2)
#Homogeneity of variance
leveneTest(residuals(m2) ~ Faba_biomass_ex$Soil * Faba_biomass_ex$Genotype)


#Potato
#two way anova without date effect
m3 <-  aov(Norg_ex_RSA ~  Genotype * Soil  , data = Potato_biomass_ex)
m3
summary(m3)
TukeyHSD(m3, which = c("Soil", "Genotype", "Date"))
# Check normality of residuals using the performance package
check_normality(m3)
#Homogeneity of variance
leveneTest(residuals(m3) ~ Potato_biomass_ex$Soil * Potato_biomass_ex$Genotype)


#Sweet potato
#two way anova without date effect
m4 <-  aov(AA_C_RSA ~  Genotype * Soil  , data = SP_biomass_ex)
m4
summary(m4)
TukeyHSD(m4, which = c("Soil", "Genotype"))
emmeans::emmeans(m4, pairwise ~ Genotype | Soil)
# Check normality of residuals using the performance package
check_normality(m4)
#Homogeneity of variance
residuals_m4 <- residuals(m4)
df_m4 <- SP_biomass_ex[rownames(model.frame(m4)), ]  
leveneTest(residuals_m4 ~ df_m4$Soil * df_m4$Genotype)

###############################
# all SxG interaction effects: 
##############################


library(emmeans)
library(multcomp)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# Compounds of interest
compounds <- c("C_ex_RSA",          # DOC
               "AA_C_RSA",          # Amino acids
               "Glucose_C_RSA_AS",  # Carbohydrates
               "CGA_C_RSA",         # Phenolics
               "unknown_C_RSA")     # Uncharacterized

# Helper: format CLD into a table
make_pretty_table <- function(cld_df, factor1, factor2) {
  cld_df %>%
    mutate(mean_se = sprintf("%.2f ± %.2f %s", emmean, SE, .group)) %>%
    select(all_of(factor1), all_of(factor2), mean_se) %>%
    pivot_wider(names_from = !!sym(factor2), values_from = mean_se) %>%
    arrange(!!sym(factor1))
}

# Create empty Word doc
doc <- read_docx()

# Loop over all compounds
for (var in compounds) {
  f <- as.formula(paste(var, "~ Genotype * Soil"))
  model <- aov(f, data = SP_biomass_ex)
  
  # Genotype comparisons within each soil
  emms_geno <- emmeans(model, ~ Genotype | Soil)
  cld_geno <- cld(emms_geno, Letters = letters, adjust = "tukey") %>%
    as.data.frame()
  geno_table <- make_pretty_table(cld_geno, "Soil", "Genotype")
  
  # Soil comparisons within each genotype
  emms_soil <- emmeans(model, ~ Soil | Genotype)
  cld_soil <- cld(emms_soil, Letters = letters, adjust = "tukey") %>%
    as.data.frame()
  soil_table <- make_pretty_table(cld_soil, "Genotype", "Soil")
  
  # Add section for this compound
  doc <- doc %>%
    body_add_par(var, style = "heading 1") %>%
    body_add_par("Genotype comparisons within soils", style = "heading 2") %>%
    body_add_flextable(flextable(geno_table)) %>%
    body_add_par("Soil comparisons within genotypes", style = "heading 2") %>%
    body_add_flextable(flextable(soil_table)) %>%
    body_add_par("")  # blank line
}

# Save final document
print(doc, target = "SweetPotato_PostHoc.docx")















# Modified data preparation for absolute compound exudation
plot_data1 <- barley_biomass_ex %>%
  select(Genotype, Soil, Glucose_RSA, AA_RSA, CGA_RSA) %>%
  group_by(Genotype, Soil) %>%
  summarise(
    Glucose_RSA_mean = mean(Glucose_RSA, na.rm = TRUE),
    Glucose_RSA_se = sd(Glucose_RSA, na.rm = TRUE) / sqrt(n()),
    CGA_RSA_mean = mean(CGA_RSA, na.rm = TRUE),
    CGA_RSA_se = sd(CGA_RSA, na.rm = TRUE) / sqrt(n()),
    AA_RSA_mean = mean(AA_RSA, na.rm = TRUE),
    AA_RSA_se = sd(AA_RSA, na.rm = TRUE) / sqrt(n())
  ) %>%
  pivot_longer(
    cols = c(ends_with("_mean"), ends_with("_se")),
    names_to = c("Exudate", ".value"),
    names_pattern = "(.+)_(.+)"
  )

# Modified data preparation for C related compound exudation
plot_data2 <- barley_biomass_ex %>%
  select(Genotype, Soil, Glucose_C_RSA, AA_C_RSA, CGA_C_RSA, unknown_C_RSA) %>%
  group_by(Genotype, Soil) %>%
  summarise(
    Glucose_C_RSA_mean = mean(Glucose_C_RSA, na.rm = TRUE),
    Glucose_C_RSA_se = sd(Glucose_C_RSA, na.rm = TRUE) / sqrt(n()),
    CGA_C_RSA_mean = mean(CGA_C_RSA, na.rm = TRUE),
    CGA_C_RSA_se = sd(CGA_C_RSA, na.rm = TRUE) / sqrt(n()),
    AA_C_RSA_mean = mean(AA_C_RSA, na.rm = TRUE),
    AA_C_RSA_se = sd(AA_C_RSA, na.rm = TRUE) / sqrt(n()),
    unknown_C_RSA_mean = mean(unknown_C_RSA, na.rm =TRUE),
    unknown_C_RSA_se = sd(unknown_C_RSA, na.rm = TRUE) / sqrt(n()),
  ) %>%
  pivot_longer(
    cols = c(ends_with("_mean"), ends_with("_se")),
    names_to = c("Exudate", ".value"),
    names_pattern = "(.+)_(.+)"
  )
# plotting code without error bars
p <- ggplot(plot_data2, aes(x = Genotype, y = mean, fill = Exudate)) +
  geom_bar(stat = "identity",  position = "stack") +
  #geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Exudate compound")) +
  scale_fill_brewer(palette = "YlGnBu", name = "Exudate Compound",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Unknown")) +
  facet_wrap(~ Soil, scales = "free_x") +
  labs(title = "Exudation by barley genotype and soil",
       x = "Genotype", y = expression("nmol C" ~ cm^-2 ~ "RSA" ~ h^-1), fill = "Exudate") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)


# plotting with error bars
# Step 1: Reverse the order of stacking and calculate the cumulative sum
plot_data2 <- barley_biomass_ex %>%
  select(Genotype, Soil, Glucose_C_RSA, AA_C_RSA, CGA_C_RSA, unknown_C_RSA) %>%
  group_by(Genotype, Soil) %>%
  summarise(
    Glucose_C_RSA_mean = mean(Glucose_C_RSA, na.rm = TRUE),
    Glucose_C_RSA_se = sd(Glucose_C_RSA, na.rm = TRUE) / sqrt(n()),
    CGA_C_RSA_mean = mean(CGA_C_RSA, na.rm = TRUE),
    CGA_C_RSA_se = sd(CGA_C_RSA, na.rm = TRUE) / sqrt(n()),
    AA_C_RSA_mean = mean(AA_C_RSA, na.rm = TRUE),
    AA_C_RSA_se = sd(AA_C_RSA, na.rm = TRUE) / sqrt(n()),
    unknown_C_RSA_mean = mean(unknown_C_RSA, na.rm = TRUE),
    unknown_C_RSA_se = sd(unknown_C_RSA, na.rm = TRUE) / sqrt(n())
  ) %>%
  pivot_longer(
    cols = c(ends_with("_mean"), ends_with("_se")),
    names_to = c("Exudate", ".value"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  # Step 2: Reverse the order of stacking and calculate cumulative sum for stacking
  group_by(Genotype, Soil) %>%
  arrange(desc(Exudate)) %>%  # Reverse the stacking order of the bars and error bars
  mutate(cum_mean = cumsum(mean) - mean)  # Calculate cumulative position for each segment

# Step 3: Create the stacked bar plot with error bars in the correct position
p1 <- ggplot(plot_data2, aes(x = Genotype, y = mean, fill = Exudate)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2, position = "stack", width = 0.8) +  # Bar outline
  geom_errorbar(aes(ymin = cum_mean + mean, ymax = cum_mean + mean + se),  # Error bars positioned at the top of each stack
                width = 0.2, position = position_identity(), linewidth = 0.2) +  # No stacking for error bars
  guides(fill = guide_legend(title = "Exudate compound")) +
  scale_fill_brewer(palette = "YlGnBu", name = "Exudate Compound",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Unknown")) +
  facet_wrap(~ Soil, scales = "free_x") +
  labs(title = "Barley",
       x = "Genotype", y = expression("nmol C" ~ cm^-2 ~ "RSA" ~ h^-1), fill = "Exudate") +
  ylim(0, 250) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.line = element_line(linewidth = 0.2),  # Set axis line width to 0.5
        axis.ticks = element_line(linewidth = 0.2, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 0.3))  # Set facet strip box line width
  

# Display the plot
print(p1)




# Stacked barplot with error bars for Faba bean

# Step 1: Reverse the order of stacking and calculate the cumulative sum
plot_data3 <- Faba_biomass_ex %>%
  select(Genotype, Soil, Glucose_C_RSA, AA_C_RSA, CGA_C_RSA, unknown_C_RSA) %>%
  group_by(Genotype, Soil) %>%
  summarise(
    Glucose_C_RSA_mean = mean(Glucose_C_RSA, na.rm = TRUE),
    Glucose_C_RSA_se = sd(Glucose_C_RSA, na.rm = TRUE) / sqrt(n()),
    CGA_C_RSA_mean = mean(CGA_C_RSA, na.rm = TRUE),
    CGA_C_RSA_se = sd(CGA_C_RSA, na.rm = TRUE) / sqrt(n()),
    AA_C_RSA_mean = mean(AA_C_RSA, na.rm = TRUE),
    AA_C_RSA_se = sd(AA_C_RSA, na.rm = TRUE) / sqrt(n()),
    unknown_C_RSA_mean = mean(unknown_C_RSA, na.rm = TRUE),
    unknown_C_RSA_se = sd(unknown_C_RSA, na.rm = TRUE) / sqrt(n())
  ) %>%
  pivot_longer(
    cols = c(ends_with("_mean"), ends_with("_se")),
    names_to = c("Exudate", ".value"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  # Step 2: Reverse the order of stacking and calculate cumulative sum for stacking
  group_by(Genotype, Soil) %>%
  arrange(desc(Exudate)) %>%  # Reverse the stacking order of the bars and error bars
  mutate(cum_mean = cumsum(mean) - mean)  # Calculate cumulative position for each segment

# Step 3: Create the stacked bar plot with error bars in the correct position
p2 <- ggplot(plot_data3, aes(x = Genotype, y = mean, fill = Exudate)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2, position = "stack", width = 0.8) +  # Bar outline
  geom_errorbar(aes(ymin = cum_mean + mean, ymax = cum_mean + mean + se),  # Error bars positioned at the top of each stack
                width = 0.2, position = position_identity(), linewidth = 0.2) +  # No stacking for error bars
  guides(fill = guide_legend(title = "Exudate compound")) +
  scale_fill_brewer(palette = "YlGnBu", name = "Exudate Compound",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Unknown")) +
  facet_wrap(~ Soil, scales = "free_x") +
  labs(title = "Faba bean",
       x = "Genotype", y = expression("nmol C" ~ cm^-2 ~ "RSA" ~ h^-1), fill = "Exudate") +
  ylim(0, 250) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.line = element_line(linewidth = 0.2),  # Set axis line width to 0.5
        axis.ticks = element_line(linewidth = 0.2, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 0.3))  # Set facet strip box line width


# Display the plot
print(p2)



# stacked barplot with error bars for potato

# Step 1: Reverse the order of stacking and calculate the cumulative sum
plot_data4 <- Potato_biomass_ex %>%
  select(Genotype, Soil, Glucose_C_RSA, AA_C_RSA, CGA_C_RSA, unknown_C_RSA) %>%
  group_by(Genotype, Soil) %>%
  summarise(
    Glucose_C_RSA_mean = mean(Glucose_C_RSA, na.rm = TRUE),
    Glucose_C_RSA_se = sd(Glucose_C_RSA, na.rm = TRUE) / sqrt(n()),
    CGA_C_RSA_mean = mean(CGA_C_RSA, na.rm = TRUE),
    CGA_C_RSA_se = sd(CGA_C_RSA, na.rm = TRUE) / sqrt(n()),
    AA_C_RSA_mean = mean(AA_C_RSA, na.rm = TRUE),
    AA_C_RSA_se = sd(AA_C_RSA, na.rm = TRUE) / sqrt(n()),
    unknown_C_RSA_mean = mean(unknown_C_RSA, na.rm = TRUE),
    unknown_C_RSA_se = sd(unknown_C_RSA, na.rm = TRUE) / sqrt(n())
  ) %>%
  pivot_longer(
    cols = c(ends_with("_mean"), ends_with("_se")),
    names_to = c("Exudate", ".value"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  # Step 2: Reverse the order of stacking and calculate cumulative sum for stacking
  group_by(Genotype, Soil) %>%
  arrange(desc(Exudate)) %>%  # Reverse the stacking order of the bars and error bars
  mutate(cum_mean = cumsum(mean) - mean)  # Calculate cumulative position for each segment

# Step 3: Create the stacked bar plot with error bars in the correct position
p3 <- ggplot(plot_data4, aes(x = Genotype, y = mean, fill = Exudate)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2, position = "stack", width = 0.8) +  # Bar outline
  geom_errorbar(aes(ymin = cum_mean + mean, ymax = cum_mean + mean + se),  # Error bars positioned at the top of each stack
                width = 0.2, position = position_identity(), linewidth = 0.2) +  # No stacking for error bars
  guides(fill = guide_legend(title = "Exudate compound")) +
  scale_fill_brewer(palette = "YlGnBu", name = "Exudate Compound",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Unknown")) +
  facet_wrap(~ Soil, scales = "free_x") +
  labs(title = "Potato",
       x = "Genotype", y = expression("nmol C" ~ cm^-2 ~ "RSA" ~ h^-1), fill = "Exudate") +
  ylim(0, 250) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.line = element_line(linewidth = 0.2),  # Set axis line width to 0.5
        axis.ticks = element_line(linewidth = 0.2, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 0.3))  # Set facet strip box line width


# Display the plot
print(p3)



# stacked barplot with error bars for sweet potato

# Step 1: Reverse the order of stacking and calculate the cumulative sum
plot_data5 <- SP_biomass_ex %>%
  select(Genotype, Soil, Glucose_C_RSA_AS, AA_C_RSA, CGA_C_RSA, unknown_C_RSA) %>%
  group_by(Genotype, Soil) %>%
  summarise(
    Glucose_C_RSA_mean = mean(Glucose_C_RSA_AS, na.rm = TRUE),
    Glucose_C_RSA_se = sd(Glucose_C_RSA_AS, na.rm = TRUE) / sqrt(n()),
    CGA_C_RSA_mean = mean(CGA_C_RSA, na.rm = TRUE),
    CGA_C_RSA_se = sd(CGA_C_RSA, na.rm = TRUE) / sqrt(n()),
    AA_C_RSA_mean = mean(AA_C_RSA, na.rm = TRUE),
    AA_C_RSA_se = sd(AA_C_RSA, na.rm = TRUE) / sqrt(n()),
    unknown_C_RSA_mean = mean(unknown_C_RSA, na.rm = TRUE),
    unknown_C_RSA_se = sd(unknown_C_RSA, na.rm = TRUE) / sqrt(n())
  ) %>%
  pivot_longer(
    cols = c(ends_with("_mean"), ends_with("_se")),
    names_to = c("Exudate", ".value"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  # Step 2: Reverse the order of stacking and calculate cumulative sum for stacking
  group_by(Genotype, Soil) %>%
  arrange(desc(Exudate)) %>%  # Reverse the stacking order of the bars and error bars
  mutate(cum_mean = cumsum(mean) - mean)  # Calculate cumulative position for each segment

# Step 3: Create the stacked bar plot with error bars in the correct position
p4 <- ggplot(plot_data5, aes(x = Genotype, y = mean, fill = Exudate)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2, position = "stack", width = 0.8) +  # Bar outline
  geom_errorbar(aes(ymin = cum_mean + mean, ymax = cum_mean + mean + se),  # Error bars positioned at the top of each stack
                width = 0.2, position = position_identity(), linewidth = 0.2) +  # No stacking for error bars
  guides(fill = guide_legend(title = "Exudate compound")) +
  scale_fill_brewer(palette = "YlGnBu", name = "Exudate compound",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Unknown")) +
  facet_wrap(~ Soil, scales = "free_x") +
  labs(title = "Sweet potato",
       x = "Genotype", y = expression("nmol C" ~ cm^-2 ~ "RSA" ~ h^-1), fill = "Exudate") +
  ylim(0, 250) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.line = element_line(linewidth = 0.2),  # Set axis line width to 0.5
        axis.ticks = element_line(linewidth = 0.2, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 0.3))  # Set facet strip box line width


# Display the plot
print(p4)


#prepare all plots for grid 2x2 

# adjust the title and strip text font sizes
p1_modified <- p1 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
  theme(legend.position = "none")

p2_modified <- p2 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
   theme(legend.position = "none")

p3_modified <- p3 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
   theme(legend.position = "none")

p4_modified <- p4 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
   theme(legend.position = "none")



# Combine the plots into a 2x2 grid with aligned axes
(p1_modified + p2_modified) / (p3_modified + p4_modified) + 
  plot_layout(guides = 'collect') & 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 8),  # Smaller facet label text
        plot.margin = margin(t = 3, r = 3, b = 3, l = 3))  # Reduced plot margins




###Pie charts old (new ones in percentage bar plot Script)

#Barley
# Summarize total mean of each compound across all genotypes and soils
pie_data <- plot_data2 %>%
  group_by(Exudate) %>%
  summarise(total = sum(mean, na.rm = TRUE)) %>%
  mutate(
    percent = total / sum(total) * 100,
    label = paste0(round(percent, 1), "%")
  )

# Create a pie chart with percentage labels
pie_chart1 <- ggplot(pie_data, aes(x = "", y = total, fill = Exudate)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 5) +
  scale_fill_brewer(palette = "YlGnBu",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Uncharacterized")) +
  labs(title = "Barley",
       fill = "Compounds") +
  theme_void() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# Display the pie chart
print(pie_chart1)


# Faba
# Summarize total mean of each compound across all genotypes and soils
pie_data <- plot_data3 %>%
  group_by(Exudate) %>%
  summarise(total = sum(mean, na.rm = TRUE)) %>%
  mutate(
    percent = total / sum(total) * 100,
    label = paste0(round(percent, 1), "%")
  )

# Create a pie chart with percentage labels
pie_chart2 <- ggplot(pie_data, aes(x = "", y = total, fill = Exudate)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 5) +
  scale_fill_brewer(palette = "YlGnBu",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Uncharacterized")) +
  labs(title = "Faba bean",
       fill = "Compounds") +
  theme_void() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# Display the pie chart
print(pie_chart2)


#Potato
# Summarize total mean of each compound across all genotypes and soils
pie_data <- plot_data4 %>%
  group_by(Exudate) %>%
  summarise(total = sum(mean, na.rm = TRUE)) %>%
  mutate(
    percent = total / sum(total) * 100,
    label = paste0(round(percent, 1), "%")
  )

# Create a pie chart with percentage labels
pie_chart3 <- ggplot(pie_data, aes(x = "", y = total, fill = Exudate)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 5) +
  scale_fill_brewer(palette = "YlGnBu",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Uncharacterized")) +
  labs(title = "Potato",
       fill = "Compounds") +
  theme_void() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# Display the pie chart
print(pie_chart3)


#Sweet potato
# Summarize total mean of each compound across all genotypes and soils
pie_data <- plot_data5 %>%
  group_by(Exudate) %>%
  summarise(total = sum(mean, na.rm = TRUE)) %>%
  mutate(
    percent = total / sum(total) * 100,
    label = paste0(round(percent, 1), "%")
  )

# Create a pie chart with percentage labels
pie_chart4 <- ggplot(pie_data, aes(x = "", y = total, fill = Exudate)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 5) +
  scale_fill_brewer(palette = "YlGnBu",
                    labels = c("Amino Acids", "Phenolics", "Carbohydrates", "Uncharacterized")) +
  labs(title = "Sweet potato",
       fill = "Compounds") +
  theme_void() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

# Display the pie chart
print(pie_chart4)

# adjust the title and strip text font sizes
p1_modified <- pie_chart1 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
  theme(legend.position = "none")

p2_modified <- pie_chart2 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
  theme(legend.position = "none")

p3_modified <- pie_chart3 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
  theme(legend.position = "none")

p4_modified <- pie_chart4 + theme(plot.title = element_text(size = 11), strip.text = element_text(size = 8)) + 
  theme(legend.position = "none")

# Combine the plots into a 2x2 grid with aligned axes
(p1_modified + p2_modified) / (p3_modified + p4_modified) + 
  plot_layout(guides = 'collect') & 
  theme_void() & 
  theme(
    plot.title = element_text(size = 11),
    strip.text = element_text(size = 8),
    plot.margin = margin(t = 3, r = 3, b = 3, l = 3)
  )



# Norg simple barplot
library(dplyr)
library(ggplot2)
library(patchwork)

# Function to prepare Norg data for one crop
prepare_data <- function(df) {
  df %>%
    select(Genotype, Soil, Norg_ex_RSA) %>%
    group_by(Genotype, Soil) %>%
    summarise(
      mean = mean(Norg_ex_RSA, na.rm = TRUE),
      se   = sd(Norg_ex_RSA, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
}

# Function to plot with Soil as facet, fixed y-limit, thin outlines
plot_norg <- function(data, title) {
  ggplot(data, aes(x = Genotype, y = mean)) +
    geom_bar(stat = "identity", color = "black", fill = "#F8766D", width = 0.7, linewidth = 0.2) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.2, linewidth = 0.2) +
    facet_wrap(~ Soil, scales = "free_x") +
    ylim(0, 22) +
    labs(
      title = title,
      x = "Genotype",
      y = expression("nmol N" ~ cm^-2 ~ "RSA" ~ h^-1)
    ) +
    theme_classic(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8, color = "black"),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 11),
      axis.line = element_line(linewidth = 0.2, color = "black"),
      axis.ticks = element_line(linewidth = 0.2, color = "black"),
      strip.background = element_rect(color = "black", linewidth = 0.2),
      panel.border = element_blank()
    )
}

# Prepare datasets
barley_data <- prepare_data(barley_biomass_ex)
faba_data   <- prepare_data(Faba_biomass_ex)
potato_data <- prepare_data(Potato_biomass_ex)
sp_data     <- prepare_data(SP_biomass_ex)

# Create plots
p1 <- plot_norg(barley_data, "Barley")
p2 <- plot_norg(faba_data, "Faba bean")
p3 <- plot_norg(potato_data, "Potato")
p4 <- plot_norg(sp_data, "Sweet potato")

# Combine into 2x2 grid
(p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(t = 3, r = 3, b = 3, l = 3))









