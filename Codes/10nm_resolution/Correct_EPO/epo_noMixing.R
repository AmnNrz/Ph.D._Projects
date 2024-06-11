library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(ComplexUpset)
library(MASS)


setwd(paste0('/Users/aminnorouzi/Documents/GitHub/spectroscopy_paper/',
             'Codes/10nm_resolution/Correct_EPO'))
# setwd(paste0('/home/amnnrz/Documents/GitHub/',
#              'spectroscopy_paper/Codes/10nm_resolution/individual/'))
source("epo_module.R")



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


# Read raw data
Residue <- read.csv(paste0(path_to_data, 
                           "Residue.csv"),
                    header = TRUE, row.names = NULL)
Residue <- Residue[-c(1, 8)]
Residue <- dplyr::filter(Residue, Wvl >=500)


Soil <- read.csv(paste0(path_to_data, 
                        "Soil.csv"),
                 header = TRUE, row.names = NULL)
Soil <- Soil[-c(1, 8)]
Soil <- dplyr::filter(Soil, Wvl >=500)


Residue <- Residue %>%
  rename(Type = Soil)

Soil <- Soil %>%
  rename(Type = Soil)

Residue <- Residue %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))


Residue <- Residue %>%  dplyr::select(-Scan) 
Soil <- Soil %>%  dplyr::select(-Scan)


crops <- unique(Residue$Type)
soils <- unique(Soil$Type)

# Min-Max Normalization
min_max_normalize <- function(matrix) {
  min_val <- min(matrix)
  max_val <- max(matrix)
  normalized_matrix <- (matrix - min_val) / (max_val - min_val)
  return(normalized_matrix)
}


df <- Soil
type <- unique(df$Type)[1]
Dm2 <- function(df, sample){
  D <- data.frame()
  for (type in unique(df$Type)){
    df_type <- dplyr::filter(df, Type == type)
    
    # df_type <- df_type %>% distinct(Wvl, RWC, .keep_all = TRUE)
    # df_filtered <- df_filtered %>% select(-Scan)
    df_type <- df_type %>%
      pivot_wider(names_from = RWC, values_from = Reflect)
    
    df_type <- as.data.frame(df_type)
    rownames(df_type) <- df_type$Wvl
    
    X <- df_type[, 4:ncol(df_type)]
    # Convert column names to numeric and get the order
    ordered_column_indices <- order(as.numeric(names(X)))
    
    # Sort the dataframe by the ordered indices
    X <- X[, ordered_column_indices]
    
    # print(dim(X))
    # print(X)
    
    min_col <- which.min(colnames(X))
    X_wet <- X[,-min_col]
    
    D_ <- X_wet - matrix(rep(X[,min_col], ncol(X_wet)),
                         ncol = ncol(X_wet), byrow = FALSE)
    
    D_mat <- as.matrix(D_)
    D_mat <- t(D_mat)
    
    D_type <- as.matrix(D_mat)
    
    # D_type <- min_max_normalize(D_type)
    D <- rbind(D, D_type)
    
    
    D_ordered_column_indices <- order(as.numeric(rownames(D)))
    D <- D[D_ordered_column_indices, ]
    
    
    D <- as.matrix(D)
    
  }
  return(D)
}

epo_scenario2 <- function(df, sample, num_pc = 2){
  
  D <- Dm2(df, sample)
  D <- as.matrix(D)
  D <- -D
  D <- D[order(as.numeric(rownames(D))), ]
  # D <- t(D)
  
  # Perform SVD on t(D) %*% D
  svd_result <- svd(t(D) %*% D)
  # svd_result <- svd(D)
  
  U <- svd_result$u
  S <- svd_result$d
  V <- svd_result$v
  # print(dim(V))
  
  Vs <- V[, 1:num_pc]
  # print(dim(Vs))
  Q <- Vs %*% t(Vs)
  
  
  P <- diag(nrow(Q)) - Q
  P <- as.matrix(P)
  return(list(P=P, Q=Q))
  
}


results_soil <- epo_scenario2(Soil, "Soil")
Ps <- results_soil$P
Qs <- results_soil$Q
results_Residue <- epo_scenario2(Residue, "Soil")
Pr <- results_Residue$P
Qr <- results_Residue$Q


type <- unique(Soil$Type)[1]
X_transformed <- data.frame()
X_original_normal <- data.frame()
for (type in unique(Soil$Type)){
  soil_filtered <- dplyr::filter(Soil, Type==type)
  soil_filtered <- soil_filtered %>%
    pivot_wider(names_from = RWC, values_from = Reflect)
  
  X <- soil_filtered[, 4:ncol(soil_filtered)]
  
  X_P = t(X) %*% Ps
  X_Q = t(X) %*% Qs
  
  X_P <- t(X_P)
  X_P <- as.data.frame(X_P)
  rownames(X_P) <- soil_filtered$Wvl
  
  X_P <- as.data.frame(X_P)
  X_P <-X_P %>% mutate(Wvl = rownames(X_P)) %>% 
      dplyr::select("Wvl", everything())
    
  X_P <- as_tibble(X_P)
    
  X_P <- X_P %>% 
      reshape2::melt(.,id = "Wvl") %>% 
      rename(RWC = variable, Reflect = value)
  X_P$Type <- type
  X_P$Sample <- 'Soil'
  
  X_transformed <- rbind(X_transformed, X_P)
  ######################################################
  
  X_normal <- min_max_normalize(X)
  X_normal <- as.data.frame(X_normal)
  rownames(X_normal) <- soil_filtered$Wvl
  
  X_normal <- as.data.frame(X_normal)
  X_normal <-X_normal %>% mutate(Wvl = rownames(X_normal)) %>% 
    dplyr::select("Wvl", everything())
  
  X_normal <- as_tibble(X_normal)
  
  X_normal <- X_normal %>% 
    reshape2::melt(.,id = "Wvl") %>% 
    rename(RWC = variable, Reflect = value)
  X_normal$Type <- type
  X_normal$Sample <- 'Soil'
  
  X_original_normal <- rbind(X_original_normal, X_normal)
  
  
}




write.csv(X_original_normal, file = paste0(path_to_data, "Soil_original_normal.csv"),
row.names = FALSE)

write.csv(X_transformed, file = paste0(path_to_data, "Soil_Transformed.csv"),
          row.names = FALSE)

# write.csv(as.data.frame(Pr), file = paste0(path_to_data, "Pr_", mix_group_name, ".csv"),
#           row.names = FALSE)
# write.csv(as.data.frame(Ps), file = paste0(path_to_data, "Ps_", mix_group_name, ".csv"),
#           row.names = FALSE)


P  <-  Pr
#plot heatmap of P
row.names(P) <- unique(Residue$Wvl)
colnames(P) <- unique(Residue$Wvl)

# Step 1: Load libraries
library(ggplot2)
library(reshape2)

# Assuming 'P' is your matrix and already loaded
# Step 2: Prepare data (if your matrix is named 'P')
data_long <- melt(P)
names(data_long) <- c("X", "Y", "Value")

# Corrected and refined breakpoints
breakpoints_Ps <- c(-0.0255038, -0.0073811, -0.0046840, -0.0021710, 0.0001066, 0.1, 0.5, 0.9963112)
breakpoints_Pr <- c(-0.0139223, -0.0042455, -0.0023056, 0.0004914, -0.0001011, 0.9995316 )

breakpoints <- breakpoints_Pr
# Creating the heatmap with the updated breakpoints
ggplot(data_long, aes(X, Y, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("white", "blue", "red", "green"))(length(breakpoints) - 1),
                       values = scales::rescale(breakpoints)) +
  scale_x_continuous(breaks = seq(500, 2450, by = 400)) +
  scale_y_continuous(breaks = seq(500, 2450, by = 400)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Value", x = "Pr", y = "")

