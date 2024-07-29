library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)
library(broom)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')
path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/')

# Get a list of all .csv files in the directory
csv_files <- list.files(path = paste0(path_to_data, "crp_sl_index_fr/"), pattern = "\\.csv$", full.names = FALSE)

# Remove the ".csv" extension from the file names
csv_files <- sub("\\.csv$", "", csv_files)

# Extract characters after the second underscore
csv_files <- sapply(strsplit(csv_files, "_"), function(x) paste(tail(x, -2), collapse = "_"))

NDTI_df <- data.frame()
CAI_df <- data.frame()
SINDRI_df <- data.frame()
name <- unique(csv_files)[1]
for (name in unique(csv_files)) {

  crop = unlist(strsplit(name, "_"))[1]
  soil = unlist(strsplit(name, "_"))[2]
  
  NDTI_original <- read.csv(paste0(path_to_data, 'crp_sl_index_fr/', 'NDTI_Original_', name, '.csv'))
  NDTI_original$crop <- crop
  NDTI_original$soil <- soil
  NDTI_original$mix <- name
  NDTI_original$index_name <- "NDTI"
  
  NDTI_df <- rbind(NDTI_df, NDTI_original)
  
  CAI_original <- read.csv(paste0(path_to_data, 'crp_sl_index_fr/', 'CAI_Original_', name, '.csv'))
  CAI_original$crop <- crop
  CAI_original$soil <- soil
  CAI_original$mix <- name
  CAI_original$index_name <- "CAI"
  
  CAI_df <- rbind(CAI_df, CAI_original)
  
  SINDRI_original <- read.csv(paste0(path_to_data, 'crp_sl_index_fr/', 'SINDRI_Original_', name, '.csv'))
  SINDRI_original$crop <- crop
  SINDRI_original$soil <- soil
  SINDRI_original$mix <- name
  SINDRI_original$index_name <- "SINDRI"
  
  SINDRI_df <- rbind(SINDRI_df, SINDRI_original)
}

df <- rbind(NDTI_df, CAI_df, SINDRI_df)

df <- df %>% dplyr::filter(!(crop == "Weathered Canola" & index_name == "NDTI"))



process_group <- function(data) {
  # Find the minimum RWC in this subset
  min_rwc_data <- data %>% filter(RWC == min(RWC))
  
  # Fit linear model to the data with minimum RWC
  model <- lm(Fraction_Residue_Cover ~ index, data = min_rwc_data)
  
  # Predict Fraction_Residue_Cover for the entire group's data
  predictions <- predict(model, newdata = data)
  
  # Add predictions to the data
  data$Predicted_Fraction_Residue_Cover <- predictions
  
  # Calculate predictions for the minimum RWC data only
  predictions_min_rwc <- predict(model, newdata = min_rwc_data)
  
  # Since all entries with minimum RWC should have the same predicted value, take the first
  min_rwc_predicted_value <- predictions_min_rwc[1]
  
  # Calculate error ratio for all entries in the data
  data$error_ratio <- data$Predicted_Fraction_Residue_Cover / min_rwc_predicted_value
  
  # Add slope and intercept from the model
  data$Slope <- coef(model)[["index"]]
  data$Intercept <- coef(model)[["(Intercept)"]]
  
  return(data)
}


results3 <- df %>%
  group_by(index_name, soil) %>%
  group_modify(~ process_group(.x))


# Filter the DataFrame for specific RWC values
results3 <- results3 %>%
  filter(RWC %in% c(0, 0.2, 0.6, 0.8, 1))




#######################################
#######################################
#           Plot Fr ~ index 
#######################################
#######################################

idx = "SINDRI"

fr_idx_df <- results3 %>% dplyr::filter(index_name == idx)
fr_idx_df <- fr_idx_df %>% dplyr::filter(RWC == 0)


# Filter out canola
fr_idx_df <- fr_idx_df %>% dplyr::filter(crop != "Weathered Canola")

base_size = 14
sl <- unique(fr_idx_df$soil)[1]
for (sl in unique(fr_idx_df$soil)){
  
  filtered <- fr_idx_df %>% dplyr::filter(soil == sl)

  p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(RWC), shape = as.factor(crop))) +
    geom_point() + # Adds points to the plot
    geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
    facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
    scale_color_discrete(name = "RWC") +
    scale_shape_discrete(name = "Crop") + # Adds a legend for RWC
    labs(x = "Index", y = "Fraction Residue Cover") + # Labels for axes
    theme_minimal() + # Minimal theme for cleaner look
    theme(legend.position = "right",
          legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
          legend.text = element_text(size = base_size), # Legend text at base size
          plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
          axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
          axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
          panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
          plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
          panel.grid = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black"),
          legend.key.size = unit(0.5, "cm")) 
  
  # Print the plot
  print(p)
  ggsave(paste0(path_to_plots, 'Fr_index_dry_fits/',"without_canola_", sl, idx, '.png'), p, width = 10, height = 7, dpi = 300)

}

#######################################
#######################################
#######################################

##############################
          # Slope
##############################


##############################
        # Intercept
##############################


##############################
        # fr_differene
##############################
results3_fr = results3
results3_fr$act_fr <- results3$Fraction_Residue_Cover
results3_fr$pred_fr <- results3_fr$Predicted_Fraction_Residue_Cover
results3_fr$fr_ratio_ <- results3_fr$Predicted_Fraction_Residue_Cover/results3_fr$Fraction_Residue_Cover
results3_fr$fr_dif <- results3_fr$Predicted_Fraction_Residue_Cover - results3_fr$Fraction_Residue_Cover

# Plotting
sl <- unique(results3$soil)[1]
results3_fr <- results3_fr %>% dplyr::filter(soil == sl)

# Filter for RWC
results3_fr <- results3_fr %>% dplyr::filter(RWC == 0)

NDTI_df <- results3_fr %>% dplyr::filter(index_name == "NDTI")
CAI_df <- results3_fr %>% dplyr::filter(index_name == "CAI")
SINDRI_df <- results3_fr %>% dplyr::filter(index_name == "SINDRI")


# fr_ratio_plot <- ggplot(NDTI_df, aes(x = factor(act_fr), y = fr_dif, color = crop, shape = crop)) +

# df_ <- SINDRI_df
base_size <- 14
fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = fr_dif)) +
  geom_boxplot(alpha = 0.6, outlier.size = 2) +  # Setting alpha directly for point transparency
  labs(x = "actual fraction", y = "fr_difference", title = paste0(df_$index_name[1], "_across_crops")) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette for the crops
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7)) +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm")) 
  # geom_hline(yintercept=1)
print(fr_ratio_plot)
ggsave(paste0(path_to_plots, 'FR_differene_fr_act_box/', 'fr_fitted_box_', df_$index_name[1], '.png'), fr_ratio_plot, width = 10, height = 7, dpi = 300)



### Fr_difference facet across RWCs
df_ <- CAI_df
# Creating the plot
fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = fr_dif, color = factor(RWC))) +
  geom_boxplot(alpha = 0.6, outlier.size = 2) +  # Setting alpha directly for point transparency
  labs(x = "Actual Fraction", y = "FR Difference", title = paste0(df_$index_name[1], " across crops")) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm")) +
  facet_wrap(~RWC, scales = "free")  # Add this line to facet by RWC

print(fr_ratio_plot)

ggsave(paste0(path_to_plots, 'FR_differene_fr_act_box_FACET_RWC/', 'fr_fitted_box_', df_$index_name[1], '.png'), fr_ratio_plot, width = 15, height = 7, dpi = 300)


df_ <- SINDRI_df
# Creating the scatter plot
fr_ratio_plot <- ggplot(df_, aes(x = pred_fr, y = act_fr, color = factor(RWC), shape = factor(crop))) +
  geom_point(alpha = 0.6, size = 2) +  # Plot points with some transparency
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a 1:1 line in red
  labs(x = "Predicted Fraction", y = "Actual Fraction", title = paste0(df_$index_name[1], " Comparison of Predicted vs. Actual Fraction")) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette for RWC
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 6, 8)) +  # Define shapes for each crop type
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +  # Set x-axis from 0 to 1
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +  # Set y-axis from 0 to 1
  theme(
    legend.position = "right",
    legend.title = element_text(size = base_size * 1.2),  # Legend title larger than base size
    legend.text = element_text(size = base_size),  # Legend text at base size
    plot.title = element_text(size = base_size * 1.5, hjust = 0.5),  # Title larger than base size
    axis.title = element_text(size = base_size * 1.2),  # Axis titles larger than base size
    axis.text = element_text(size = base_size, color = "black"),  # Axis text at base size
    panel.background = element_rect(fill = "white", colour = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = "white"),  # Set plot background to white
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    legend.key.size = unit(0.5, "cm")
  ) +
  facet_wrap(~RWC, scales = "free")  # Facet by RWC

print(fr_ratio_plot)
ggsave(paste0(path_to_plots, 'FR_pred_fr_act_FACET_/', 'FR_pred_fr_act_FACET_', df_$index_name[1], '.png'), fr_ratio_plot, width = 13, height = 7, dpi = 300)

