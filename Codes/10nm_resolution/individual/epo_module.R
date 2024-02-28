#' EPO Module
#' 
#' @docType module
#' @name epo_module
#' @aliases my_module
#' @title My Module
#' @description This module contains the External Parameter Orthogonalisation (EPO) function(s).
#' @details ...
#' @author Amin Norouzi
#' @seealso \code{\link{...}}
#' @importFrom ...
#' @export ...
#' 
#' # Load the module
#' source("epo_module.R")
#'
#' # Call the function
#' transformed_df <- epo(df)


#' @param df Original reflectances dataframe input.
#' @param num_pc Number of principle components
#' @return Transformed reflectances dataframe output.
#' @examples ...
#' df <- data.frame(ID=1:3, Sample=c('Residue', 'Residue', 'Residue'),
#'                           Type=c('Canola', 'Canola', 'Canola'),
#'                           RWC=c(1, 1, 1),
#'                           Wvl=c(1060, 1070, 1080),
#'                           Reflect=c(12.3, 13.4, 13.5))
#' 
#' @include epo_module.R

library(tibble)
library(reshape2)

library(tidyverse)
library(dplyr)
library(ggplot2)

# # 
# # path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
# #                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
# #                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# 
# Residue_Median <- read.csv(paste0(path_to_data,
#                                   "Residue.csv"),
#                            header = TRUE, row.names = NULL)
# Residue_Median <- Residue_Median[-c(1, 8)]
# 
# Soil_Median <- read.csv(paste0(path_to_data,
#                                "Soil.csv"),
#                         header = TRUE, row.names = NULL)
# Soil_Median <- Soil_Median[-c(1, 8)]
# 
# Residue_Median <- Residue_Median %>%
#   rename(Type = Soil)
# 
# Soil_Median <- Soil_Median %>%
#   rename(Type = Soil)
# 
# Residue_Median <- Residue_Median %>%
#   mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))
# Dataframe <- Soil_Median
# # Dataframe <- Residue_Median
# type <- 'Athena'
# # type <- 'Canola'
# num_pc <- 1

epo <- function(Dataframe, num_pc = 1){
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
    X <- X[, ncol(X):1]
    
    min_col <- which.min(colnames(X))
    X_wet <- X[,-min_col]
    
    D <- X_wet - matrix(rep(X[,min_col], ncol(X_wet)),
                        ncol = ncol(X_wet), byrow = FALSE)
    
    D_mat <- as.matrix(D)
    D_mat <- -D_mat
    
    svd_result <- svd(t(D_mat) %*% D_mat)
    
    U <- svd_result$u
    S <- svd_result$d
    V <- svd_result$v
    
    Vs <- t(V)[, 1:num_pc]
    Q <- Vs %*% t(Vs)
    
    # P <- diag(nrow(Q)) - Q
    P <- matrix(1, nrow = nrow(Q), ncol = nrow(Q)) - Q
    
    X_wet <- as.matrix(X_wet)
    P <- as.matrix(P)
    
    X_transformed <- X_wet %*% P
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

