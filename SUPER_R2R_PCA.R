
############################################################################
# With rna seq data BOTH TREATMENTS
############################################################################


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
library(FactoMineR)

##load all the R2R WP2 data

#Barley
setwd("~/Root2Res/R2R - Results/R_with_Carmen")
setwd ("C:/Users/Henning/Documents/Root2Res/R2R - Results/R_with_Carmen/Barley_biomass")
getwd()
barley_biomass <- read.delim("Barley_biomass.txt", sep = "\t")
head(barley_biomass)

#Faba
setwd ("C:/Users/Henning/Documents/Root2Res/R2R - Results/Barley/R_with_Carmen/Faba_biomass")
getwd()
Faba_biomass <- read.delim("Faba_biomass.txt", sep = "\t")
Faba_biomass_no_planet = subset(Faba_biomass, !Genotype == "Planet")

#Potato
setwd ("C:/Users/Henning/Documents/Root2Res/R2R - Results/Barley/R_with_Carmen/Potato_biomass")
getwd()
Potato_biomass <- read.delim("Potato_biomass.txt", sep = "\t")
Potato_biomass_no_planet = subset(Potato_biomass, !Genotype == "Planet")

#Sweet Potato 
setwd ("C:/Users/Henning/Documents/Root2Res/R2R - Results/Barley/R_with_Carmen/SP_biomass")
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
                                     c("24. AD_Irina","27. BF_Fairytale","60. BF_Irina","61. AD_Irina",
                                       "63. BF_Planet","65. ARV_Fairytale","66. AD_Planet",
                                       "68. BF_Laureate","69. BF_Laureate","70. BF_Planet",
                                       "71. AD_Laureate","73. BF_Fairytale","74. AD_Fairytale",
                                       "75. ARV_Irina"),]



#faba: remove strong outlier (45.ILB due to low RSSR!)
Faba_biomass_ex = Faba_biomass_no_planet[!Faba_biomass_no_planet$Sample_ID %in% c("45. ILB_AD", "20. Zoran_AD", "26. Zoran_BF", "32. ILB_AR"  ),] #"20. Zoran_AD", "26. Zoran_BF","32. ILB_AR"

#potato: remove outlier 75. Duke of York_ARV
Potato_biomass_ex = Potato_biomass_no_planet[!Potato_biomass_no_planet$Sample_ID %in% c("75. Duke of York_ARV", "15. Cara_BF", 
                                                                                        "50. Innovator_ARV", "62. Duke of York_BF", 
                                                                                        "74. Desiree_BF",  
                                                                                        "88. Duke of York_ARV"),]

#SP: 2. 4. nitrogen (and AA) contaminated, 40. glucose contaminated, 14. and 24 C_ex affected (RSSR) 
# never more than 1 replicate out of 5 affected
SP_biomass_ex = SP_biomass_no_planet[!SP_biomass_no_planet$Sample_ID %in%
                                       c("2. Minamiyutaka_ARV", "4. Minamiyutaka_BF",
                                         "40. Blesbok_BF", "14. Blesbok_ADAS",
                                         "24. Blesbok_ARV", "37. Blesbok_BF"),] # 37. Blesbok_BF




str(barley_biomass_ex)
str(Faba_biomass_ex)
str(Potato_biomass_ex)
str(SP_biomass_ex)


# Remove specific columns
#Barley <- Barley[, !names(Barley) %in% c("DMA_nmol_cm2_h", "MA_nmol_cm2_h", "epi.HMA_nmol_cm2_h", "epi.HDMA_nmol_cm2_h")]
#Barley <- Barley[, !names(Barley) %in% c("sumPS_ex_cm2")]


#Plan A : Orientation based on von Haden & Michael (bergmann + only total C)

Barley <- barley_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                                       C_ex_RSA, Norg_ex_RSA, RTD, SRL, avg_dia_mm, 
)

Faba <- Faba_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                                   C_ex_RSA, Norg_ex_RSA, RTD, SRL, avg_dia_mm, 
)

# to be excluded if bergmann parameters involved: "20. Zoran_AD", "26. Zoran_BF","32. ILB_AR"


Potato <- Potato_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                                       C_ex_RSA, Norg_ex_RSA, RTD, SRL, avg_dia_mm,  
)

SP <- SP_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                               C_ex_RSA, Norg_ex_RSA, RTD, SRL, avg_dia_mm, 
)
# to be excluded if bergmann paramters involved: 37. Blesbok_BF




#Plan B : Focus on plant growth vs exudation rate

#Barley <- barley_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                                       C_ex_RSA, Norg_ex_RSA, Glucose_RSA, CGA_RSA, AA_RSA, unknown_RSA, RL_m, RSA, 
)

#Faba <- Faba_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                                   C_ex_RSA, Norg_ex_RSA, Glucose_RSA, CGA_RSA, AA_RSA, unknown_RSA, RL_m, RSA ,
)

#Potato <- Potato_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                                       C_ex_RSA, Norg_ex_RSA, Glucose_RSA, CGA_RSA, AA_RSA, unknown_RSA, RL_m, RSA,
)

#SP <- SP_biomass_ex %>% select(Sample_ID, Genotype, Soil,root_total_DW, shoot_total_DW, root_shoot, 
                               C_ex_RSA, Norg_ex_RSA, Glucose_RSA_AS, CGA_RSA, AA_RSA, unknown_RSA,RL_m, RSA,
)




## Prepare the data
Barley <- na.omit(Barley)
Faba <- na.omit(Faba)
Potato <- na.omit(Potato)
SP <- na.omit(SP)

## Check out the dataset
colnames(Barley)
dim(Barley)
str(Barley)



# ONLY MAIN EFFECTS (find script for all effects SUPER_R2R_EDI)
library(factoextra)
## PCA with FactoMineR package ## 

#perform PCA with numeric subset
pca_barley_factominer <- PCA(X = Barley[, -c(1:3)])

## finalize Biplot PCA ## 

                          
# Biplot for soil 
# Define custom colors for each soil category
soil_colors <- c("#0072B2", "#E69F00", "#009E73") 

# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_barley <- fviz_pca_biplot(
  pca_barley_factominer,
  geom.ind = "point",
  fill.ind = Barley$Soil,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = soil_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Barley") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-6, 6))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_barley$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_barley$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_barley$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}

                   
# Print the biplot
print(biplot_barley)



#FABA BEAN 

## PCA with FactoMineR package ## 

#perform PCA with numeric subset
pca_faba_factominer <- PCA(X = Faba[, -c(1:3)])


# Biplot for Genotype
# Define custom colors for each soil category
genotype_colors <- c("#999999", "#56B4E9", "#F0E442", "orchid4") 

# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_faba<- fviz_pca_biplot(
  pca_faba_factominer,
  geom.ind = "point",
  fill.ind = Faba$Genotype,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = genotype_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Faba bean") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-6, 6))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_faba$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_faba$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_faba$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_faba)



## PCA with FactoMineR package ## 

#perform PCA with numeric subset
pca_potato_factominer <- PCA(X = Potato[, -c(1:3)])


## finalize Biplot PCA ## 
# Biplot for Genotype
# Define custom colors for each soil category
genotype_colors <- c("#999999", "#56B4E9", "#F0E442", "orchid4") 


# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_potato<- fviz_pca_biplot(
  pca_potato_factominer,
  geom.ind = "point",
  fill.ind = Potato$Genotype,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = genotype_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Potato") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-6, 6))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_potato$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_potato$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_potato$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_potato)




#SWEET POTATO

# PCA with FactoMineR package ## 

#perform PCA with numeric subset
pca_sp_factominer <- PCA(X = SP[, -c(1:3)])

# Biplot for Genotype
# Define custom colors for each soil category
genotype_colors <- c("#999999", "#56B4E9", "#F0E442", "orchid4") 


# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
library(ggplot2)

library(ggplot2)

# Create the biplot with ellipses
biplot_SP <- fviz_pca_biplot(
  pca_sp_factominer,
  geom.ind = "point",
  fill.ind = SP$Genotype,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = genotype_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Sweet potato") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-6, 6))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_SP$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_SP$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_SP$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}

print(biplot_SP)














# EVERYTHING AGAIN BUT NOT WITH CENTERED AXIS LIMITIS # 


# ONLY MAIN EFFECTS (find script for all effects SUPER_R2R_EDI)
library(factoextra)
## PCA with FactoMineR package ## 



## finalize Biplot PCA ## 


# Biplot for soil 
# Define custom colors for each soil category
soil_colors <- c("#0072B2", "#E69F00", "#009E73") 

# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_barley <- fviz_pca_biplot(
  pca_barley_factominer,
  geom.ind = "point",
  fill.ind = Barley$Soil,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = soil_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Barley") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_barley$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_barley$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_barley$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_barley)



#FABA BEAN 

## PCA with FactoMineR package ## 



# Biplot for Genotype
# Define custom colors for each soil category
genotype_colors <- c("#999999", "#56B4E9", "#F0E442", "orchid4") 

# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_faba<- fviz_pca_biplot(
  pca_faba_factominer,
  geom.ind = "point",
  fill.ind = Faba$Genotype,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = genotype_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Faba bean") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_faba$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_faba$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_faba$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_faba)



## PCA with FactoMineR package ## 




## finalize Biplot PCA ## 
# Biplot for Genotype
# Define custom colors for each soil category
genotype_colors <- c("#999999", "#56B4E9", "#F0E442", "orchid4") 


# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_potato<- fviz_pca_biplot(
  pca_potato_factominer,
  geom.ind = "point",
  fill.ind = Potato$Genotype,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = genotype_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Potato") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_potato$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_potato$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_potato$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_potato)




#SWEET POTATO

# PCA with FactoMineR package ## 

# Biplot for Genotype
# Define custom colors for each soil category
genotype_colors <- c("#999999", "#56B4E9", "#F0E442", "orchid4") 




# Create the biplot with ellipses
biplot_SP <- fviz_pca_biplot(
  pca_sp_factominer,
  geom.ind = "point",
  fill.ind = SP$Genotype,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = genotype_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Sweet potato") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )  

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_SP$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_SP$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_SP$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}

print(biplot_SP)






##### TO BE DONE: other effect for the supplements (not the main effect) ####


# Biplot for Genotype
# Define custom colors for each soil category
genotype_colors <- c("#999999", "#56B4E9", "#F0E442", "orchid4") 


# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_barley <- fviz_pca_biplot(
  pca_barley_factominer,
  geom.ind = "point",
  fill.ind = Barley$Genotype,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = genotype_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Barley") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-5, 5))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_barley$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_barley$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_barley$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_barley)



# Define custom colors for each soil category
soil_colors <- c("#0072B2", "#E69F00", "#009E73") 

# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_faba<- fviz_pca_biplot(
  pca_faba_factominer,
  geom.ind = "point",
  fill.ind = Faba$Soil,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = soil_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Faba bean") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-5, 5))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_faba$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_faba$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_faba$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_faba)


# Create the biplot with PCA_barley_factorminer data and prepared palette of colors
biplot_potato<- fviz_pca_biplot(
  pca_potato_factominer,
  geom.ind = "point",
  fill.ind = Potato$Soil,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = soil_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Potato") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-5, 5))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_potato$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_potato$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_potato$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}


# Print the biplot
print(biplot_potato)



# Create the biplot with ellipses
biplot_SP <- fviz_pca_biplot(
  pca_sp_factominer,
  geom.ind = "point",
  fill.ind = SP$Soil,
  col.ind = "black",
  pointshape = 21,
  pointsize = 2,
  palette = soil_colors,
  addEllipses = TRUE,
  col.var = "black"
) +
  ggtitle("Sweet potato") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-6.2, 6.2), ylim = c(-5, 5))

# Find the ellipse layer by class (stat_ellipse)
ellipse_layer_index <- which(sapply(biplot_SP$layers, function(x) inherits(x$stat, "StatEllipse")))

# If found, remove the outline by setting color = NA and keep fill
if(length(ellipse_layer_index) > 0){
  biplot_SP$layers[[ellipse_layer_index]]$aes_params$colour <- NA  # remove outline
  biplot_SP$layers[[ellipse_layer_index]]$aes_params$color <- NA   # safety for spelling differences
  # Keep fill as is (do not modify fill)
}

print(biplot_SP)
