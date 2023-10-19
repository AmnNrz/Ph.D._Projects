library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

mixed_original <- read.csv(paste0(path_to_data, "mixed_original.csv"), check.names = FALSE)

mixed_original$Type <- paste0(
  mixed_original$cropType, "_", mixed_original$soilType)

# mixed_original$Scan <- paste0(
#   mixed_original$cropScan, "_", mixed_original$soilScan)

mixed_original <- mixed_original %>%
  mutate(RWC_ave = (
    mixed_original$cropRWC + mixed_original$soilRWC) / 2) 

mixed_original <- mixed_original %>% 
  select(-Fr, everything(), Fr) %>% 
  select("500":ncol(mixed_original)) %>%
  select("Type", "RWC_ave", "Fr", everything())

mixed_original <- mixed_original %>% 
  pivot_longer(cols = '500':names(mixed_original)[ncol(mixed_original)],
               names_to = 'Wvl',
               values_to = 'Reflect') 

#############################################
t <- dplyr::filter(mixed_original, Type== "Canola_Athena")



#############################################

fr1 <- dplyr::filter(mixed_original, Fr == unique(mixed_original$Fr)[1])

fr1 <- fr1 %>% 
  pivot_wider(names_from = RWC_ave, values_from = Reflect)

fr2 <- dplyr::filter(mixed_original, Fr == unique(mixed_original$Fr)[2])

fr2 <- fr2 %>% 
  pivot_wider(names_from = RWC_ave, values_from = Reflect)

#######    Pr  & Ps

# Read raw data
Residue <- read.csv(paste0(path_to_data, "Residue.csv"))
Residue <- Residue[, -c(1, ncol(Residue))] %>% 
  mutate(Sample = ifelse(is.character('Crop Residue'), 'Residue', Sample))

Soil <- read.csv(paste0(path_to_data, "Soil.csv"))
Soil <- Soil[, -c(1, ncol(Soil))]

soil_unique_rwc_count <- Soil %>%
  group_by(Soil) %>%
  summarise(Unique_RWC_Count = n_distinct(RWC))

res_unique_rwc_count <- Residue %>%
  group_by(Crop) %>%
  summarise(Unique_RWC_Count = n_distinct(RWC))

# Rename Crop and Soil columns to Type
Residue <- Residue %>%
  rename(Type = Crop)
Soil <- Soil %>%
  rename(Type = Soil)

# Select common Wvls
Residue <- Residue[Residue$Wvl %in% Soil$Wvl, ]
Soil <- Soil[Soil$Wvl %in% Residue$Wvl, ]
length(unique(Residue$Wvl))
length(unique(Soil$Wvl))


# Calculate Pr and Ps
crp <- "Bagdad"
Residue <- Residue %>% select(-Scan) 
Soil <- Soil %>% select(-Scan)

# EPO Function
epo <- function(df, crp){
  
  df <- dplyr::filter(df, Type == crp) %>% 
  pivot_wider(names_from = RWC, values_from = Reflect) 

  df <- as.data.frame(df)
  rownames(df) <- df$Wvl
  
  X <- df[, 4:ncol(df)]/100
  
  min_col <- which.min(colnames(X))
  X_wet <- X[,-min_col]
  
  D <- X_wet - matrix(rep(X[,min_col], ncol(X_wet)), ncol = ncol(X_wet), byrow = FALSE)
  D <- -D
  
  D_mat <- as.matrix(D)
  # Perform SVD on t(D) %*% D
  svd_result <- svd(t(D_mat) %*% D_mat)
  
  U <- svd_result$u
  S <- svd_result$d
  V <- svd_result$v
  
  Vs <- V[, 1:2]
  Q <- Vs %*% t(Vs)
  
  P <- diag(nrow(Q)) - Q
  return(P)
  
}


Pr <- epo(Residue, crp="Peas")
Ps <- epo(Soil, crp="Athena")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  















