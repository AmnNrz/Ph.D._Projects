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

# EPO function
epo <- function(Dataframe, num_pc = 2){
  transformed_DF <- data.frame()
  for (type in unique(Dataframe$Type)){
    df <- dplyr::filter(Dataframe, Type==type)
    df <- df %>% distinct(Wvl, RWC, .keep_all = TRUE)
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
    
    # Compute the projection matrix Q using the first r columns of U
    Q <- U[, 1:r] %*% t(U[, 1:r])
    
    # P <- diag(nrow(Q)) - Q
    P <- matrix(1, nrow = nrow(Q), ncol = nrow(Q)) - Q
    
    X_wet <- as.matrix(X_wet)
    P <- as.matrix(P)
    
    # X_transformed <- P %*% X_wet
    X_transformed <- t(X_wet) %*% P
    X_transformed <- t(X_transformed)
    X <- as.data.frame(X)
    X_transformed <- as.data.frame(X_transformed)
    row.names(X_transformed) <- row.names(X)
    colnames(X_transformed) <- colnames(X_wet)
    X_transformed <- rownames_to_column(X_transformed, var = 'Wvl')
    X_transformed <- melt(X_transformed, id.vars = "Wvl", variable.name = "RWC",
                          value.name = "Reflect")
    
    X_transformed$Type <- type
    
    X_transformed$Sample <- Dataframe$Sample[1]
    X_transformed$Wvl <- as.numeric(as.character(X_transformed$Wvl))
    
    transformed_DF <- rbind(transformed_DF, X_transformed)
  }
  return(transformed_DF)
}





