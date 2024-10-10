library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)
library(broom)
library(gridExtra)


# path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
#                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')
path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')


# path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                         'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
#                         'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/01/')
path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/01/')

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
df <- rbind(NDTI_df, CAI_df, SINDRI_df)
df <- rbind(NDTI_df, CAI_df, SINDRI_df)

# Add fresh/weathered column 
fresh <- c("Canola", "Garbanzo Beans", "Peas", "Wheat Norwest Duet", 
           "Wheat Pritchett")

weathered <- c("Weathered Canola", "Weathered Wheat")
df <- df %>% 
  mutate(age = case_when(
    crop %in% fresh ~ "fresh", 
    crop %in% weathered ~ "weathered"
  ))

# Remove weathered wheat and weathered canola
fresh_df <- df %>% dplyr::filter(!(crop == "Weathered Canola" | crop == "Weathered Wheat"))


fit_on_dry <- function(data) {
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

# Fit on driest 
results <- df %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry(.x))


# Filter the DataFrame for specific RWC values
results <- results %>%
  filter(RWC %in% c(0, 0.2, 0.4, 0.6, 0.8, 1))

results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ratio_ <- results_fr$Predicted_Fraction_Residue_Cover/results_fr$Fraction_Residue_Cover
results_fr$fr_dif <- results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover


# Filter for RWC = 0
df_to_plot <- results_fr %>% dplyr::filter(RWC == 0)

# Filter for fresh crops
df_to_plot <- df_to_plot %>% dplyr::filter(age == "fresh")
base_size = 14

# Define custom colors (one for each crop)
custom_colors <- c("Canola" = "#fcca46", "Garbanzo Beans" = "#233d4d", "Peas" = "#fe7f2d", 
                   "Wheat Norwest Duet" = "#619b8a", "Wheat Pritchett" = "#a1c181")  


for (sl in unique(df_to_plot$soil)){
  for (idx_ in unique(df_to_plot$index_name)){
    filtered <- df_to_plot %>% dplyr::filter(soil == sl)
    filtered <- filtered %>% dplyr::filter(index_name == idx_)
    
    # Calculate slopes for each crop
    slopes <- filtered %>%
      group_by(crop) %>%
      summarise(slope = coef(lm(Fraction_Residue_Cover ~ index))[2])
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes, by = "crop") %>%
      mutate(crop_label = paste0(crop, " (", round(slope, 2), ")"))
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop))) +
      geom_point(size = 4) + # Adds points to the plot
      geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Crop residue (slope)", values = custom_colors,
                         labels = filtered$crop_label) +
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(legend.position = "right",
            legend.title = element_text(size = base_size * 2), # Legend title larger than base size
            legend.text = element_text(size = base_size * 1.5), # Legend text at base size
            axis.title = element_text(size = base_size * 2), # Axis titles larger than base size
            axis.text = element_text(size = base_size * 1.5, color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_blank(),
            strip.text = element_blank()) +
      scale_y_continuous(limits = c(0, 1)) 
    
    # Print the plot
    print(p)
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/', sl, "_", filtered$index_name[1], '.png'), p, width = 10, height = 7, dpi = 300)
  }
}

