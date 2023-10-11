# Load the reshape2 package
library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(minpack.lm)


# Set the path to directory
# Set the path to directory
# path_to_directory <- paste0("/Users/aminnorouzi/Library/CloudStorage/", 
#                           "OneDrive-WashingtonStateUniversity(email.wsu.edu)/",
#                           "Ph.D/Projects/Spectroscopy_Paper/Data/10nm_res_individual/",
#                           "index_org_trsfed_crp_sl")
# 
path_to_directory <- paste0("/home/amnnrz/OneDrive - a.norouzikandelati/",
                            "Ph.D/Projects/Spectroscopy_Paper/Data/",
                            "10nm_res_individual/index_org_trsfed_crp_sl/")

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Spectroscopy_Paper/Plots/', 
                        '10nm_res_individual/frAct_frEst/')

# Get a list of all .csv files in the directory
all_csv_files <- list.files(path = path_to_directory, pattern = "\\.csv$", full.names = FALSE)

# Remove the ".csv" extension from the file names
csv_file_fullname <- sub("\\.csv$", "", all_csv_files)

# Extract characters after the second underscore
CROOP_soil_names <- sapply(strsplit(csv_file_fullname, "_"), function(x) paste(tail(x, -2), collapse = "_"))
print(unique(CROOP_soil_names))

CROOPs <- unique(sapply(strsplit(CROOP_soil_names, "_"), function(x) paste(x[1])))
soils <- unique(sapply(strsplit(CROOP_soil_names, "_"), function(x) paste(x[2])))

# setwd(paste0("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive",
#               "-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper",
#               "/Spectrometry-main copy EPO"))


################################################################
##################                          ####################
################################################################
index = 'SINDRI'

all_data <- list()
for (croop in CROOPs){
  # remaining_CROOPs <- setdiff(CROOPs, CROOP)
  df_original <- data.frame()
  for (crp in CROOPs){
    for (sl in soils){
      index_df <- read.csv(paste0(path_to_directory, index, '_Original_', crp, '_', paste0(sl), '.csv'))
      index_df$soil <- sl
      index_df$CROOP <- crp
      df_original <- rbind(df_original, index_df)
      
    }
  }
  
  df_original_minRWC_CROOP <- dplyr::filter(df_original, CROOP == croop) %>%
    group_by(CROOP) %>%
    mutate(min_rwc = min(RWC.y)) %>%  # Calculate the min RWC.y for each CROOP type
    filter(RWC.y == min_rwc) %>%      # Keep rows where RWC.y equals the minimum
    ungroup() 
  
  df_original_minRWC_CROOP$Slope = NA
  df_original_minRWC_CROOP$Intercept = NA
  
  # Perform linear regression (Fr ~ index)
  for (rwc in unique(df_original_minRWC_CROOP$RWC.y)){
    df <- dplyr::filter(df_original_minRWC_CROOP, RWC.y == rwc)
    lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
                 data = df)
    slope <- coef(lm_fit)[2]
    intercept <- coef(lm_fit)[1]
    df_original_minRWC_CROOP <- df_original_minRWC_CROOP %>%
      mutate(Slope = ifelse(RWC.y == rwc, slope,Slope)) %>% 
      mutate(Intercept = ifelse(RWC.y == rwc, intercept, Intercept))
  } 
  
  df_original$Slope_base <- df_original_minRWC_CROOP$Slope[1]
  df_original$Intercept_base <- df_original_minRWC_CROOP$Intercept[1]
  df_original$Fr_est <- df_original$Intercept_base + 
    df_original$Slope_base * df_original$CAI
  
  # Load the dplyr package
  library(dplyr)
  
  # plot fr_act ~fr_est
  df_original <- df_original %>%
    mutate(
      rwc_range = case_when(
        RWC.y < 0.25  ~ "RWC < 25%",
        RWC.y >= 0.25 & RWC.y <= 0.75 ~ "25% < RWC < 75%",
        RWC.y > 0.75  ~ "RWC > 75%",
        TRUE ~ "Other" # This will catch any NA or other unexpected values
      )
    )
  
  
  df_original$rwc_range <- factor(df_original$rwc_range, levels = c("RWC < 25%", "25% < RWC < 75%", "RWC > 75%"))
  
  lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_original)
  
  # Add a new column to df_original to store the predicted values
  df_original$Predictions <- NA  # Initialize with NA
  
  # Populate this column with the predicted values
  df_original$Predictions[!is.na(df_original$Fr_est)] <- predict(lm_model, newdata = df_original[!is.na(df_original$Fr_est),])
  
  # Calculate residuals and RMSE only for the rows for which predictions were actually made
  valid_rows <- !is.na(df_original$Predictions) & !is.na(df_original$Fraction_Residue_Cover)
  
  # Calculate residuals
  residuals <- df_original$Fraction_Residue_Cover[valid_rows] - df_original$Predictions[valid_rows]
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2))
  print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))
  
  df_original$RMSE <- round(rmse, 4)
  
  # Create the scatter plot
  p1 <- ggplot(df_original, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
    geom_point(aes(color = rwc_range), size = 0.6) +
    geom_smooth(method = 'lm', se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
    labs(title = paste0("NDTI - ", croop),
         x = "Fraction residue cover", y ="Estimated fraction residue cover") +
    theme(legend.title = "RWC",
          plot.title = element_text(hjust = 0.5))+
    theme_minimal() +
    # scale_color_viridis(discrete = TRUE, option = "E") +
    scale_x_continuous(limits = c(0, 1)) + # Change as needed
    scale_y_continuous(limits = c(0, 1)) + # Change as needed
    coord_fixed(ratio = 1)
  p1 <- p1 + annotate("text", x = 0.85, y = 0.05, label = paste("RMSE:", round(rmse, 4)), hjust=1)
  print(p1)
  
  
  df_original$INDEX <- 'NDTI'
  df_original$Method <- 'After EPO'
  df_original_NDTI <- df_original
  
  
  ##########################################################################
  ######################        Fr_es after EPO       ######################
  ##########################################################################
  
  
  df_transformed <- data.frame()
  for (crp in CROOPs){
    for (sl in soils){
      index_df <- read.csv(paste0(path_to_directory, index, '_transformed_', crp, '_', paste0(sl), '.csv'))
      index_df$soil <- sl
      index_df$CROOP <- crp
      df_transformed <- rbind(df_transformed, index_df)
      
    }
  }
  # 
  df_transformed_minRWC_CROOP <- dplyr::filter(df_transformed, CROOP == croop) %>%
    group_by(CROOP) %>%
    mutate(min_rwc = min(RWC.y)) %>%  # Calculate the min RWC.y for each CROOP type
    filter(RWC.y == min_rwc) %>%      # Keep rows where RWC.y equals the minimum
    ungroup()
  
  df_transformed_minRWC_CROOP$Slope = NA
  df_transformed_minRWC_CROOP$Intercept = NA
  
  # Perform linear regression (Fr ~ index)
  for (rwc in unique(df_transformed_minRWC_CROOP$RWC.y)){
    df <- dplyr::filter(df_transformed_minRWC_CROOP, RWC.y == rwc)
    lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
                 data = df)
    slope <- coef(lm_fit)[2]
    intercept <- coef(lm_fit)[1]
    df_transformed_minRWC_CROOP <- df_transformed_minRWC_CROOP %>%
      mutate(Slope = ifelse(RWC.y == rwc, slope,Slope)) %>%
      mutate(Intercept = ifelse(RWC.y == rwc, intercept, Intercept))
  }
  
  df_transformed$Slope_base <- df_transformed_minRWC_CROOP$Slope[1]
  df_transformed$Intercept_base <- df_transformed_minRWC_CROOP$Intercept[1]
  df_transformed$Fr_est_transformedBaseline <- df_transformed$Intercept_base +
    df_transformed$Slope_base * df_transformed$CAI
  
  df_original_wet <- df_original %>%
    group_by(CROOP) %>%
    mutate(min_rwc = min(RWC.y)) %>%  # Calculate the min RWC.y for each CROOP type
    filter(RWC.y != min_rwc) %>%      # remove rows where RWC.y equals the minimum
    ungroup() 
  
  df_transformed$Fr_est_originalBaseline <- df_original_wet$Intercept_base + 
    df_original_wet$Slope_base * df_transformed$CAI
  
  df_transformed$Fr_est <- df_original_wet$Intercept_base + 
    df_original_wet$Slope_base * df_transformed$CAI
  # Load the dplyr package
  library(dplyr)
  
  # plot fr_act ~fr_est
  df_transformed <- df_transformed %>%
    mutate(
      rwc_range = case_when(
        RWC.y < 0.25  ~ "RWC < 25%",
        RWC.y >= 0.25 & RWC.y <= 0.75 ~ "25% < RWC < 75%",
        RWC.y > 0.75  ~ "RWC > 75%",
        TRUE ~ "Other" # This will catch any NA or other unexpected values
      )
    )
  
  
  df_transformed$rwc_range <- factor(df_transformed$rwc_range, levels = c("RWC < 25%", "25% < RWC < 75%", "RWC > 75%"))
  
  lm_model <- lm(Fraction_Residue_Cover ~ Fr_est_originalBaseline, data = df_transformed)
  
  # Add a new column to df_original to store the predicted values
  df_transformed$Predictions <- NA  # Initialize with NA
  
  # Populate this column with the predicted values
  df_transformed$Predictions[!is.na(df_transformed$Fr_est_originalBaseline)] <-
    predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est_originalBaseline),])
  
  # Calculate residuals and RMSE only for the rows for which predictions were actually made
  valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)
  
  # Calculate residuals
  residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2))
  print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))
  
  df_transformed$RMSE <- round(rmse, 4)
  
  # Create the scatter plot
  p2 <- ggplot(df_transformed, aes(y = Fr_est_originalBaseline, x = Fraction_Residue_Cover)) +
    geom_point(aes(color = rwc_range), size = 0.6) +
    geom_smooth(method = 'lm', se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
    labs(title = paste0("NDTI - ", croop),
         x = "Fraction residue cover", y ="Estimated fraction residue cover") +
    theme(legend.title = "RWC",
          plot.title = element_text(hjust = 0.5))+
    theme_minimal() +
    # scale_color_viridis(discrete = TRUE, option = "E") +
    scale_x_continuous(limits = c(-20, 20)) + # Change as needed
    scale_y_continuous(limits = c(-20, 20)) + # Change as needed
    coord_fixed(ratio = 1)
  p2 <- p2 + annotate("text", x = 0.85, y = 0.05, label = paste("RMSE:", round(rmse, 4)), hjust=1)
  print(p2)
  
  
  df_transformed$INDEX <- 'NDTI'
  df_transformed$Method <- 'After EPO'
  df_transformed_NDTI <- df_transformed
  
  # Adding a new column to each dataset to denote source
  df_original_selected <- df_original %>% select(Fraction_Residue_Cover, Fr_est, rwc_range, CROOP, soil, RMSE)
  df_transformed_selected <- df_transformed %>% select(Fraction_Residue_Cover, Fr_est, rwc_range, CROOP, soil, RMSE)
  df_original_selected$source <- "Original"
  df_transformed_selected$source <- "EPO"
  
  # Combining datasets
  df_combined <- rbind(df_original_selected,df_transformed_selected)
  
  # Add new column for the CROOP
  df_combined$CROOP <- croop
  
  # Add to the list
  all_data[[croop]] <- df_combined
  
}

# ##########################################################################
# ##########################################################################
# ##########################################################################

for (df in all_data){
  rmse_values <- c("Original" = dplyr :: filter(df, source == 'Original')['RMSE'][1,1],
                   "EPO" = dplyr :: filter(df, source == 'EPO')['RMSE'][1,1]
  )
  
  # Create a data frame with the positions and labels for the RMSE annotations
  rmse_df <- data.frame(
    source = names(rmse_values),
    x = 0.7,
    y = 0.1,
    label = paste0("RMSE: ", rmse_values)
  )
  rmse_df$source <- factor(rmse_df$source, levels = c("Original", "EPO"))
  
  df$source <- factor(df$source, levels = c("Original", "EPO"))
  
  # library(forcats)
  # df$source <- fct_relevel(df$source, c("Original", "EPO"))
  
  
  p <- ggplot(df, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
    geom_point(aes(color = rwc_range), size = 0.6) +
    geom_smooth(method = 'lm', se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") +
    labs(title = paste0("NDTI - ", df['CROOP'][1, 1]),
         x = "Fraction residue cover", y = "Estimated fraction residue cover") +
    theme(legend.title = element_text("RWC"),
          plot.title = element_text(hjust = 0.5)) +
    theme_minimal() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_fixed(ratio = 1) +
    facet_wrap(~source, ncol = 2) + # Add this for faceting
    # Add RMSE annotations using geom_text and the rmse_df data frame
    geom_text(data = rmse_df, aes(x = x, y = y, label = label), inherit.aes = FALSE, size=3)
  
  
  print(p)
  print(paste0(path_to_plots, df['CROOP'][1, 1],".png"))
  ggsave(paste0(path_to_plots, df['CROOP'][1, 1],".png"), p,
         width = 45, height = 15, units = "cm",dpi = 200)
  
}
