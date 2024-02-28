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
epo <- function(Dataframe, num_pc){
  transformed_DF <- data.frame()
  for (type in unique(Dataframe$Type)){
    df <- dplyr::filter(Dataframe, Type==type)
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
    
    Vs <- V[, 1:num_pc]
    Q <- Vs %*% t(Vs)
    
    # P <- diag(nrow(Q)) - Q
    P <- matrix(1, nrow = nrow(Q), ncol = nrow(Q)) - Q
    X_transformed <- X %*% P
    transformed_DF <- rbind(transformed_DF, X_transformed)
  }
  return(transformed_DF)
}

