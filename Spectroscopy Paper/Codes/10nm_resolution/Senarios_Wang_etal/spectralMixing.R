library(tidyverse)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

Residue <- read.csv(paste0(path_to_data, "Residue.csv"))
Residue <- Residue[, -c(1, ncol(Residue))]
Soil <- read.csv(paste0(path_to_data, "Soil.csv"))
Soil <- Soil[, -c(1, ncol(Soil))]

res_wide <- Residue %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 



# Cross join
combined_data <- merge(Residue, Soil, by = NULL)

# Filter rows with close RWC values
close_values <- combined_data %>% 
  filter(abs(RWC.x - RWC.y) <= 5) %>%
  select(soil, crop, RWC.x, RWC.y)

print(close_values)
