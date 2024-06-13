library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal_correct/')

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
Residue <- read.csv(paste0(path_to_data, "Residue.csv"))
Residue <- Residue[, -c(1, ncol(Residue))] %>% 
  mutate(Sample = ifelse(is.character('Crop Residue'), 'Residue', Sample))

Soil <- read.csv(paste0(path_to_data, "Soil.csv"))
Soil <- Soil[, -c(1, ncol(Soil))]

# Rename Crop and Soil columns to Type
Residue <- Residue %>%
  rename(Type = Soil)
Soil <- Soil %>%
  rename(Type = Soil)

# Select common Wvls
Residue <- Residue[Residue$Wvl %in% Soil$Wvl, ]
Soil <- Soil[Soil$Wvl %in% Residue$Wvl, ]
length(unique(Residue$Wvl))
length(unique(Soil$Wvl))

# df <- Res_rwc_filtered

# EPO Function
epo <- function(df){
  
  df <- df %>% 
    pivot_wider(names_from = RWC, values_from = Reflect) 
  
  df <- as.data.frame(df)
  rownames(df) <- df$Wvl
  
  X <- df[, 4:ncol(df)]
  
  min_col <- which.min(colnames(X))
  X_wet <- X[,-min_col]
  
  D <- X_wet - matrix(rep(X[,min_col], ncol(X_wet)),
                      ncol = ncol(X_wet), byrow = FALSE)
  
  D_mat <- as.matrix(D)
  D_mat <- t(D_mat)
  # Perform SVD on t(D) %*% D
  svd_result <- svd(D_mat)
  
  U <- svd_result$u
  S <- svd_result$d
  V <- svd_result$v
  
  Vs <- V[, 1:1]
  Q <- Vs %*% t(Vs)

  P <- diag(nrow(Q)) - Q
  # P <- matrix(1, nrow = nrow(Q), ncol = nrow(Q)) - Q
  return(P)
  
}

Residue <- Residue %>% select(-Scan) 
Soil <- Soil %>% select(-Scan)

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
    
    
    Pr <- epo(Res_rwc_filtered)
    Ps <- epo(Soil_rwc_filtered)
    
    fr <- unique(mixed_original_filtered$Fraction)[1]
    
    Xsr_HAT <- data.frame()
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
    
      
      Xsr_hat <- 1/2 * 
        (t(as.matrix(Xsr_)) %*% as.matrix(Ps) %*% as.matrix(Pr) + 
           t(as.matrix(Xsr_)) %*% as.matrix(Pr) %*% as.matrix(Ps))  
      
      Xsr_hat <- t(Xsr_hat)
      
      Xsr_hat <- Xsr_hat + abs(min(Xsr_hat))
      
      rownames(Xsr_hat) <- rownames(Xsr_) 
      
      Xsr_hat <- as.data.frame(Xsr_hat)
      
      Xsr_hat <-Xsr_hat %>% mutate(Wvl = rownames(Xsr_hat)) %>% 
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




