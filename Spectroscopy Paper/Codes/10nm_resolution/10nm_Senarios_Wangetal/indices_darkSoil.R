library(tidyverse)
library(dplyr)
library(ggplot2)

# 
# path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
#                        '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/allCrops_darkSoil/')

# path_to_plot <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
#                        '/Ph.D/Projects/Spectroscopy_Paper/Plots/mixed_Wangetal/')

path_to_plot <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Spectroscopy_Paper/Plots/mixed_Wangetal/allCrops_darkSoil/')

Xsr_Original <- read.csv(paste0(path_to_data, "Xsr_Original.csv"))
Xsr_Transformed <- read.csv(paste0(path_to_data, "Xsr_Transformed.csv"))
                            
Xsr_Original <- Xsr_Original %>% 
  select(c("Mix", "Fraction", "crop_rwc", "Wvl", "Reflect")) %>% 
  rename(RWC = crop_rwc)

Xsr_Original <- Xsr_Original[names(Xsr_Transformed)]

# create dataframe for "Spectra ~ Wvl" before/after EPO plot 
Xsr_Org <- Xsr_Original
Xsr_Org$source <- 'Original'
Xsr_Trans <- Xsr_Transformed
Xsr_Trans$source <- 'EPO'

Xsr_combined <- rbind(Xsr_Org, Xsr_Trans)
write.csv(Xsr_combined, file = paste0(path_to_data, "Xsr_combined.csv"), row.names = FALSE)

# Select required Wvl
target_wavelengths <- c(2200, 2000, 2100, 2260, 1660, 1600, 2330)

# Calculate indices using original reflectance 
Xsr_Original_Wvl <- Xsr_Original %>%
  dplyr::filter(Wvl %in% target_wavelengths)

Xsr_Original_indices <- Xsr_Original_Wvl %>%
  spread(Wvl, Reflect) 

Xsr_Original_indices$CAI <- 
  (0.5 * (Xsr_Original_indices$`2000` + Xsr_Original_indices$`2200`) - 
     Xsr_Original_indices$`2100`)

Xsr_Original_indices$SINDRI <- 
  (Xsr_Original_indices$`2200` - Xsr_Original_indices$`2260`) / 
  (Xsr_Original_indices$`2200` + Xsr_Original_indices$`2260`)

Xsr_Original_indices$NDTI <- 
  (Xsr_Original_indices$`1660` - Xsr_Original_indices$`2330`) / 
  (Xsr_Original_indices$`1660` + Xsr_Original_indices$`2330`)

Xsr_Original_indices$R2220 <- Xsr_Original_indices$`2200`/Xsr_Original_indices$`2000`
Xsr_Original_indices$R1620 <- Xsr_Original_indices$`1600`/Xsr_Original_indices$`2000`
Xsr_Original_indices$RSWIR <- Xsr_Original_indices$`1660`/Xsr_Original_indices$`2260`
Xsr_Original_indices$ROLI <- Xsr_Original_indices$`1660`/Xsr_Original_indices$`2330`




# Calculate indices using transformed reflectance 
Xsr_Transformed_Wvl <- Xsr_Transformed %>%
  dplyr::filter(Wvl %in% target_wavelengths)

Xsr_Transformed_indices <- Xsr_Transformed_Wvl %>%
  spread(Wvl, Reflect) 

Xsr_Transformed_indices$CAI <- 
  (0.5 * (Xsr_Transformed_indices$`2000` + Xsr_Transformed_indices$`2200`) - 
     Xsr_Transformed_indices$`2100`)

Xsr_Transformed_indices$SINDRI <- 
  (Xsr_Transformed_indices$`2200` - Xsr_Transformed_indices$`2260`) / 
  (Xsr_Transformed_indices$`2200` + Xsr_Transformed_indices$`2260`)

Xsr_Transformed_indices$NDTI <- 
  (Xsr_Transformed_indices$`1660` - Xsr_Transformed_indices$`2330`) / 
  (Xsr_Transformed_indices$`1660` + Xsr_Transformed_indices$`2330`)

Xsr_Transformed_indices$R2220 <- Xsr_Transformed_indices$`2200`/Xsr_Transformed_indices$`2000`
Xsr_Transformed_indices$R1620 <- Xsr_Transformed_indices$`1600`/Xsr_Transformed_indices$`2000`
Xsr_Transformed_indices$RSWIR <- Xsr_Transformed_indices$`1660`/Xsr_Transformed_indices$`2260`
Xsr_Transformed_indices$ROLI <- Xsr_Transformed_indices$`1660`/Xsr_Transformed_indices$`2330`



Xsr_Original_indices$source <- 'Original'
Xsr_Transformed_indices$source <- 'EPO'

Xsr_Original_Transformed_indices <- rbind(Xsr_Original_indices, Xsr_Transformed_indices)


write.csv(Xsr_Original_Transformed_indices, file = paste0(path_to_data, "Xsr_Original_Transformed_indices.csv"), 
          row.names = FALSE)





