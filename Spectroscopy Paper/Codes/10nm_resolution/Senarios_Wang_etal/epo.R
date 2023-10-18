library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

mixed_original <- read.csv(paste0(path_to_data, "mixed_original.csv"), check.names = FALSE)

mixed_original$Type <- paste0(
  mixed_original$cropType, "_", mixed_original$soilType)

mixed_original$Scan <- paste0(
  mixed_original$cropScan, "_", mixed_original$soilScan)

mixed_original <- mixed_original %>%
  mutate(RWC_ave = (
    mixed_original$cropRWC + mixed_original$soilRWC) / 2) 

mixed_original <- mixed_original %>% 
  select("500":ncol(mixed_original)) %>%
    select("Type", "RWC_ave", "Scan", everything())



