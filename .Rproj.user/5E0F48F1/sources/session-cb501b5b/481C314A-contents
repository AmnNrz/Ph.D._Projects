library(tidyverse)
library(dplyr)
library(ggplot2)
source("epo_module.R")

setwd(paste0('/Users/aminnorouzi/Documents/GitHub/spectroscopy_paper/',
             'Codes/10nm_resolution/individual'))
# setwd(paste0('/home/amnnrz/Documents/GitHub/',
#              'spectroscopy_paper/Codes/10nm_resolution/individual/'))

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# 
# path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/')

mixed_original <- read.csv(paste0(path_to_data, 'mixed_original.csv'),
                           check.names = FALSE)

mixed_original$Type <- paste0(
  mixed_original$Crop, "_", mixed_original$Soil)

mixed_original <- mixed_original %>%
  mutate(RWC_ave = (
    mixed_original$crop_rwc + mixed_original$soil_rwc) / 2) 
mixed_original$Scan <- mixed_original$Scan

mixed_original <- mixed_original %>% 
  select(-Fraction, everything(), Fraction) %>% 
  select(c("crop_rwc", "soil_rwc", "Scan", "500":ncol(mixed_original))) %>%
  select("Type", "RWC_ave", "Fraction", "Scan", everything())

mixed_original <- mixed_original %>% 
  pivot_longer(cols = '500':names(mixed_original)[ncol(mixed_original)],
               names_to = 'Wvl',
               values_to = 'Reflect') 

# Read raw data
Residue <- read.csv(paste0(path_to_data, 
                                  "Residue.csv"),
                           header = TRUE, row.names = NULL)
Residue <- Residue[-c(1, 8)]

Soil <- read.csv(paste0(path_to_data, 
                               "Soil.csv"),
                        header = TRUE, row.names = NULL)
Soil <- Soil[-c(1, 8)]

Residue <- Residue %>%
  rename(Type = Soil)

Soil <- Soil %>%
  rename(Type = Soil)

Residue <- Residue %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))


# Select common Wvls
Residue <- Residue[Residue$Wvl %in% Soil$Wvl, ]
Soil <- Soil[Soil$Wvl %in% Residue$Wvl, ]
length(unique(Residue$Wvl))
length(unique(Soil$Wvl))

# Residue <- Residue %>% select(-Scan) 
# Soil <- Soil %>% select(-Scan)

# Calculate Xsr_hat
crops <- unique(Residue$Type)
soils <- unique(Soil$Type)

crp <- crops[1]
sl <- soils[1]

Xsr_transformed <- data.frame()
for (crp in crops){
  for(sl in soils){
    # filter original and mixed_original by crop and soil
    Res_rwc_filtered <- dplyr::filter(Residue, Type==crp)
    Soil_rwc_filtered <- dplyr::filter(Soil, Type==sl)
    mixed_original_filtered <- 
      dplyr::filter(mixed_original, Type== paste0(crp, "_", sl))
    
    
    Res_rwc_filtered <- Res_rwc_filtered[
      Res_rwc_filtered$RWC %in% mixed_original_filtered$crop_rwc, 
    ]
    
    Soil_rwc_filtered <- Soil_rwc_filtered[
      Soil_rwc_filtered$RWC %in% mixed_original_filtered$soil_rwc, 
    ]
    
    
    Pr <- project(Res_rwc_filtered)
    Ps <- project(Soil_rwc_filtered)
    
    fr <- unique(mixed_original_filtered$Fraction)[1]
    
    Xsr_HAT <- data.frame()
    fr <- unique(mixed_original_filtered$Fraction)[1]
    for (fr in unique(mixed_original_filtered$Fraction)){
      
      Xsr <- dplyr::filter(mixed_original_filtered, Fraction == fr)
      Xsr <- Xsr %>% 
        select(-c(Scan, soil_rwc, RWC_ave)) %>% 
        
        # rwc values in Xsr would be crop rwc. Therefore, in the Xsr_transformed
        # we will have crop_rwc not soil 
        pivot_wider(names_from = crop_rwc, values_from = Reflect) 
      
      Xsr <- as.data.frame(Xsr)
      rownames(Xsr) <- Xsr$Wvl
      
      Xsr_ <- Xsr %>% 
        select(-c("Type", "Fraction", "Wvl"))
      Xsr_ <- Xsr_ %>% 
        select(-as.character((min(as.numeric(names(Xsr_))))))
      
      Xsr_ <- Xsr_[, ncol(Xsr_):1]
      
      Xsr_hat <- 1/2 *
        (as.matrix(Xsr_) %*% as.matrix(Ps) %*% as.matrix(Pr) + 
           as.matrix(Xsr_) %*% as.matrix(Pr) %*% as.matrix(Ps))  
      
      Xsr_hat <- t(Xsr_hat)/10
      
      # Xsr_hat <- Xsr_hat + abs(min(Xsr_hat))
      
      rownames(Xsr_hat) <- colnames(Xsr_)
      
      Xsr_hat <- as.data.frame(Xsr_hat)
      
      Xsr_hat <- as.data.frame(t(Xsr_hat))
      
      Xsr_hat <- Xsr_hat %>% mutate(Wvl = rownames(Xsr_hat)) %>% 
        select("Wvl", everything())
      
      Xsr_hat <- as_tibble(Xsr_hat)
      
      Xsr_hat <- Xsr_hat %>% 
        reshape2::melt(.,id = "Wvl") %>% 
        rename(RWC = variable, Reflect = value)
        # pivot_longer(cols = names(Xsr_hat[,-1]), names_to = 'RWC',
        #              values_to = 'Reflect') 
      Xsr_hat <- cbind(Fraction = Xsr$Fraction[1], Xsr_hat) 
      Xsr_hat <- cbind(Mix = Xsr$Type[1], Xsr_hat)  
      
      Xsr_HAT <- rbind(Xsr_HAT, Xsr_hat)
    }
    Xsr_transformed <- rbind(Xsr_transformed, Xsr_HAT)
  }
}

write.csv(Xsr_transformed, file = paste0(path_to_data, "Xsr_Transformed.csv"),
          row.names = FALSE)
  

mixed_original <- mixed_original %>% 
  rename(Mix = Type)

write.csv(mixed_original, file = paste0(path_to_data, "Xsr_Original.csv"),
          row.names = FALSE)




