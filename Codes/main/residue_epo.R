setwd(paste0('/home/amnnrz/Documents/GitHub/spectroscopy_paper/Codes/main/'))
library(tidyverse)
library(dplyr)
library(ggplot2)
source("epo_module_new.R")



path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

residue_df <- read.csv(paste0(path_to_data, 
                                  "Residue.csv"),
                           header = TRUE, row.names = NULL)
residue_df <- residue_df[-c(1, 8)]

residue_df <- rename(residue_df, Crop = Soil)  

residue_df <- residue_df %>%
  rename(Type = Crop)
  


# epo
type <- unique(residue_df$Type)[5]
num_pc = 2
Dataframe <- residue_df
epo <- function(Dataframe, num_pc = 2){
  transformed_DF <- data.frame()
  for (type in unique(Dataframe$Type)){
    df <- dplyr::filter(Dataframe, Type==type)
    df <- df %>% distinct(Wvl, RWC, .keep_all = TRUE)
    df <- df %>% select(-Scan)
    df <- df %>% 
      pivot_wider(names_from = RWC, values_from = Reflect) 
    
    df <- as.data.frame(df)
    rownames(df) <- df$Wvl
    
    X <- df[, 4:ncol(df)]
    # Convert column names to numeric and order the columns
    X <- X[, order(as.numeric(names(X)))]
    X <- X[, ncol(X):1]
    
    min_col <- which.min(colnames(X))
    X_wet <- X[,-min_col]
    
    D <- X_wet - matrix(rep(X[,min_col], ncol(X_wet)),
                        ncol = ncol(X_wet), byrow = FALSE)
    
    D_mat <- as.matrix(D)
    # D_mat <- -D_mat
    
    svd_result <- svd(D_mat)
    # svd_result <- svd(D_mat %*% t(D_mat))

    
    # U matrix (left singular vectors)
    U <- svd_result$u
    
    # Sigma (diagonal matrix of singular values)
    Sigma <- svd_result$d
    
    # Compute the rank of A based on non-zero singular values
    r <- sum(Sigma > 1e-10)  # Threshold to avoid numerical issues
    
    # Compute the projection matrix P using the first r columns of U
    Q <- U[, 1:r] %*% t(U[, 1:r])
    
    # Print the projection matrix P
    print(Q)
 
    # P <- diag(nrow(Q)) - Q
    P <- matrix(1, nrow = nrow(Q), ncol = nrow(Q)) - Q
    
    X_wet <- as.matrix(X_wet)
    P <- as.matrix(P)
    
    X_transformed <- X_wet %*% P
    # X_transformed <- t(X_wet) %*% P
    # X_transformed <- t(X_transformed)
    X <- as.data.frame(X)
    X_transformed <- as.data.frame(X_transformed)
    row.names(X_transformed) <- row.names(X)
    colnames(X_transformed) <- colnames(X_wet)
    X_transformed <- rownames_to_column(X_transformed, var = 'Wvl')
    X_transformed <- melt(X_transformed, id.vars = "Wvl", variable.name = "RWC",
                          value.name = "Reflect")
    org_df_subset <- subset(Dataframe, Type == type)
    org_df_subset$Scan_RWC <- paste(
      as.character(org_df_subset$Scan), as.character(org_df_subset$RWC),
      sep = "_")
    scan_list <- unique(org_df_subset$Scan_RWC)
    
    
    
    mapping_list <- strsplit(scan_list, "_")
    mapping_df <- do.call(rbind, lapply(
      mapping_list, function(x) data.frame(Scan = x[1], RWC = as.numeric(x[2]))))
    
    mapping_dict <- setNames(mapping_df$Scan, mapping_df$RWC)
    
    
    X_transformed$Scan <- mapping_dict[as.character(X_transformed$RWC)]
    
    
    X_transformed <- X_transformed %>% select(Scan, everything())
    
    X_transformed$Type <- type
    
    X_transformed$Sample <- Dataframe$Sample[1]
    
    transformed_DF <- rbind(transformed_DF, X_transformed)
  }
  return(transformed_DF)
}









M <- Q
#plot heatmap of P
row.names(M) <- unique(Residue$Wvl)
colnames(M) <- unique(Residue$Wvl)

# Step 1: Load libraries
library(ggplot2)
library(reshape2)

# Assuming 'P' is your matrix and already loaded
# Step 2: Prepare data (if your matrix is named 'P')
data_long <- melt(M)
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

