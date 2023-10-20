library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Data/',
                       '10nm_Senarios_Wangetal/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

path_to_plot <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Plots/mixed_Wangetal/')

Xsr_combined_indices <- read.csv(paste0(path_to_data,
                                        "Xsr_Original_Transformed_indices.csv"))

##################################################################
######################
######################        One Crop All Soils
######################
##################################################################

split_mix <- strsplit(Xsr_combined_indices$Mix, "_")
Xsr_combined_indices$Crop <- sapply(split_mix, "[[", 1)
Xsr_combined_indices$Soil <- sapply(split_mix, "[[", 2)

crp <- unique(Xsr_combined_indices$Crop)[5]


for (crp in unique(Xsr_combined_indices$Crop)){
  subset_df <- Xsr_combined_indices[
    grep(paste0("^", crp, "_"), Xsr_combined_indices$Mix), ] 
  
  
  dry_df_original <- dplyr::filter(subset_df, source == "Original")
  dry_df_original <- dplyr::filter(dry_df_original, RWC == min(subset_df$RWC))
  
  index <- "CAI"
  # Perform linear regression (Fr ~ index)
  lm_fit <- lm(dry_df_original$Fraction ~ dry_df_original[[index]],
               data = dry_df_original)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  
  original_df <- dplyr::filter(
    Xsr_combined_indices, source == "Original"
  )
  
  original_df$Slope_base <- slope
  original_df$Intercept_base <- intercept
  original_df$Fr_est <- original_df$Intercept_base + 
    original_df$Slope_base * original_df[[index]]
  
  
  # plot fr_act ~fr_est
  original_df <- original_df %>%
    mutate(
      rwc_range = case_when(
        RWC < 0.25  ~ "RWC < 25%",
        RWC >= 0.25 & RWC <= 0.75 ~ "25% < RWC < 75%",
        RWC > 0.75  ~ "RWC > 75%",
        TRUE ~ "Other" # This will catch any NA or other unexpected values
      )
    )
  
  original_df$rwc_range <- factor(
    original_df$rwc_range, levels = c(
      "RWC < 25%", "25% < RWC < 75%", "RWC > 75%"))
  
  lm_model <- lm(Fraction ~ Fr_est, data = original_df)
  
  # Add a new column to original_df to store the predicted values
  original_df$Predictions <- NA  # Initialize with NA
  
  # Populate this column with the predicted values
  original_df$Predictions[!is.na(original_df$Fr_est)] <- 
    predict(lm_model,
            newdata = original_df[!is.na(original_df$Fr_est),])
  
  # Calculate residuals and RMSE only for the rows for which predictions were actually made
  valid_rows <- !is.na(original_df$Predictions) & 
    !is.na(original_df$Fraction)
  
  # Calculate residuals
  residuals <- original_df$Fraction[valid_rows] - original_df$Predictions[valid_rows]
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2))
  print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))
  
  original_df$RMSE <- round(rmse, 10)
  
  
  # Create the scatter plot
  p1 <- ggplot(original_df, aes(y = Fr_est, x = Fraction)) +
    geom_point(aes(color = rwc_range), size = 0.6) +
    geom_smooth(method = 'lm', se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
    labs(title = paste0("NDTI - ", crp),
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
  
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  
  dry_df_EPO <- dplyr::filter(subset_df, source == "EPO")
  dry_df_EPO <- dplyr::filter(dry_df_EPO, RWC == min(dry_df_EPO$RWC))
  
  
  # Perform linear regression (Fr ~ index)
  lm_fit <- lm(dry_df_EPO$Fraction ~ dry_df_EPO[[index]],
               data = dry_df_EPO)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  
  EPO_df <- dplyr::filter(
    Xsr_combined_indices, source == "EPO"
  )
  
  EPO_df$Slope_base <- slope
  EPO_df$Intercept_base <- intercept
  EPO_df$Fr_est <- EPO_df$Intercept_base + 
    EPO_df$Slope_base * EPO_df[[index]]
  
  
  # plot fr_act ~fr_est
  EPO_df <- EPO_df %>%
    mutate(
      rwc_range = case_when(
        RWC < 0.25  ~ "RWC < 25%",
        RWC >= 0.25 & RWC <= 0.75 ~ "25% < RWC < 75%",
        RWC > 0.75  ~ "RWC > 75%",
        TRUE ~ "Other" # This will catch any NA or other unexpected values
      )
    )
  
  EPO_df$rwc_range <- factor(
    EPO_df$rwc_range, levels = c(
      "RWC < 25%", "25% < RWC < 75%", "RWC > 75%"))
  
  lm_model <- lm(Fraction ~ Fr_est, data = EPO_df)
  
  # Add a new column to EPO_df to store the predicted values
  EPO_df$Predictions <- NA  # Initialize with NA
  
  # Populate this column with the predicted values
  EPO_df$Predictions <- predict(lm_model, newdata = EPO_df)
  
  # Calculate residuals
  residuals <- EPO_df$Fraction - EPO_df$Predictions
  
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2))
  print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))
  
  EPO_df$RMSE <- round(rmse, 10)
  
  
  # Create the scatter plot
  p2 <- ggplot(EPO_df, aes(y = Fr_est, x = Fraction)) +
    geom_point(aes(color = rwc_range), size = 0.6) +
    geom_smooth(method = 'lm', se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
    labs(title = paste0("NDTI - ", crp),
         x = "Fraction residue cover", y ="Estimated fraction residue cover") +
    theme(legend.title = "RWC",
          plot.title = element_text(hjust = 0.5))+
    theme_minimal() +
    # scale_color_viridis(discrete = TRUE, option = "E") +
    scale_x_continuous(limits = c(0, 1)) + # Change as needed
    scale_y_continuous(limits = c(0, 1)) + # Change as needed
    coord_fixed(ratio = 1)
  p2 <- p2 + annotate("text", x = 0.85, y = 0.05, label = paste("RMSE:", round(rmse, 4)), hjust=0.5)
  print(p2)
  
  
  
  
  
  df <- rbind(original_df, EPO_df)
  
  df$source <- factor(df$source, levels = c("Original", "EPO"))
  
  p_combined <- ggplot(df, aes(y = Fr_est, x = Fraction)) +
    geom_point(aes(color = rwc_range), size = 0.6) +
    geom_smooth(method = 'lm', se = FALSE) +
    geom_abline(
      intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
    labs(title = paste0(
      "NDTI performance before and after EPO using regression for - ", crp),
         x = "Fraction residue cover", y ="Estimated fraction residue cover", 
      color = "RWC range") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),        
          panel.grid.minor = element_blank(),       
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_line(color = "black"),
          strip.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"))+
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_fixed(ratio = 1) +
    geom_text(data = df, aes(
      x = 0.85, y = 0.05, label = paste("RMSE:", round(RMSE, 4)),
      hjust=0.5), inherit.aes = FALSE, show.legend = FALSE) +
    facet_wrap(~ source, ncol = 2)
  
  print(p_combined)
  
  ggsave(filename = paste0(path_to_plot, "frEst_Act/oneCrop_forAll/", index, "_", crp, ".png"), plot = p_combined, dpi = 200, width = 14, height = 5, units = "in")
  
}


