library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)
library(broom)
library(gridExtra)
library(stringr)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')
# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/',
#                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')


path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/01/')
# path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/01/')

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
fresh <- c("Canola", "Garbanzo Beans", "Peas", "Wheat Norwest Duet")

weathered <- c("Weathered Canola", "Weathered Wheat")
df <- df %>% 
  mutate(age = case_when(
    crop %in% fresh ~ "fresh", 
    crop %in% weathered ~ "weathered"
  ))

# Remove weathered wheat and weathered canola
fresh_df <- df %>% dplyr::filter(crop %in% fresh)

filtered_fresh <- fresh_df %>% dplyr::filter(index_name == "NDTI" & crop == "Canola"
                                             & RWC == 0)

# Filter the DataFrame for specific RWC values
df <- df %>%
  filter(RWC %in% c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Filter for RWC = 0
dry_df <- df %>% dplyr::filter(RWC == 0)

# Filter for fresh crops
df_to_plot <- dry_df %>% dplyr::filter(age == "fresh")
base_size = 14


# Define custom colors (one for each crop)
custom_colors <- c("Canola" = "#fcca46", "Garbanzo Beans" = "#233d4d", "Peas" = "#fe7f2d", 
                   "Wheat" = "#619b8a", "Wheat Pritchett" = "#a1c181")  


for (sl in unique(df_to_plot$soil)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    filtered <- filtered %>%
      mutate(crop = if_else(crop == "Wheat Norwest Duet", "Wheat", crop))
    
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "crop") %>%
      mutate(
        crop_label = paste0(
          crop, " (", round(slope_value, 2), ",", round(intercept_value, 2) ,")"
          )
        )

    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop))) +
      geom_point(size = 2.5) + # Adds points to the plot
      # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "fixed") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Crop residue (slope, intercept)", values = custom_colors,
                         labels = local_crop_labels) +
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size * 0.8), # Legend text at base size
            axis.title = element_text(size = base_size), # Axis titles larger than base size
            axis.text = element_text(size = base_size * 0.8, color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_blank(),
            strip.text = element_blank()) +
      scale_y_continuous(limits = c(0, 1))
      # coord_fixed(ratio = 1)
    
    # Print the plot
    print(p)
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/fresh/', sl, "_", filtered$index_name[1], '.png'), 
           p, width = 7, height = 3.5, dpi = 300)
  }
}

###########################
# Plot Fresh vs weathered for canola and wheat
###########################

crops <- c("Canola", "Weathered Canola", "Wheat Norwest Duet", "Weathered Wheat")
df_to_plot <- dry_df %>% dplyr::filter(crop %in% crops)

# Rename specific crop values in the 'crop' column of df_to_plot
df_to_plot <- df_to_plot %>%
  mutate(crop = recode(crop,
                       "Canola" = "Fresh canola",
                       "Weathered Canola" = "Weathered canola",
                       "Wheat Norwest Duet" = "Fresh wheat",
                       "Weathered Wheat" = "Weathered wheat"))


# Specify the desired order of crops for the legend
desired_order <- c("Fresh canola", "Weathered canola", "Fresh wheat", "Weathered wheat")

# Update df_to_plot to set the order of the factor levels for 'crop'
df_to_plot <- df_to_plot %>%
  mutate(crop = factor(crop, levels = desired_order))

custom_colors <- c("Fresh canola" = "#fe7f2d", "Weathered canola" = "#fe7f2d",
                   "Fresh wheat" = "#619b8a", "Weathered wheat" = "#619b8a"
                   )  
# Define the shapes: default shapes for normal crops, different shapes for weathered crops
custom_shapes <- c("Fresh canola" = 16, "Weathered canola" = 17, 
                   "Fresh wheat" = 16, "Weathered wheat" = 17)

for (sl in unique(df_to_plot$soil)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "crop") %>%
      mutate(crop_label = paste0(
        crop, " (", round(slope_value, 2), ",", round(intercept_value, 2) ,")"
      ))
    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop), shape = as.factor(crop))) +
      geom_point(size = 4) + # Adds points to the plot
      # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Crop residue (slope)", values = custom_colors,
                         labels = local_crop_labels) +
      scale_shape_manual(name = "Crop residue (slope)", values = custom_shapes, labels = local_crop_labels) +
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size * 0.8), # Legend text at base size
            axis.title = element_text(size = base_size), # Axis titles larger than base size
            axis.text = element_text(size = base_size * 0.8, color = "black"), # Axis text at base size
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
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/by_age/', sl, "_", filtered$index_name[1], '.png'), 
           p, width = 7, height = 3.5, dpi = 300)
  }
}


###########################
# Plot fr ~ index across soils
###########################
# Filter for RWC = 0
dry_df <- df %>% dplyr::filter(RWC == 0)
df_to_plot <- dry_df
custom_colors <- c("Athena" = "#582f0e", "Bagdad" = "#7f4f24", "Benwy"= "#936639",
                   "Broadax"= "#a68a64", "Endicott"= "#b6ad90", "Lance"= "#c2c5aa",
                   "Mondovi 1" = "#a4ac86", "Mondovi 2" = "#656d4a", "Oxy" = "#414833",
                   "Palouse" = "#333d29", "Ritzville" = "#774936", "Shano" =  "#580c1f")  


for (crp in unique(df_to_plot$crop)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(crop == crp, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(soil) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "soil") %>%
      mutate(soil_label = paste0(
        soil, " (", round(slope_value, 2), ",", round(intercept_value, 2) ,")")
        )
    
    # Dynamically create the labels for the legend
    local_soil_labels <- filtered %>%
      distinct(soil, soil_label) %>%
      pull(soil_label, soil)
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(soil))) +
      geom_point(size = 4) + # Adds points to the plot
      # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Soil (slope)", values = custom_colors,
                         labels = local_soil_labels) +
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size * 0.8), # Legend text at base size
            axis.title = element_text(size = base_size), # Axis titles larger than base size
            axis.text = element_text(size = base_size * 0.8, color = "black"), # Axis text at base size
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
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/soil/', crp, "_", filtered$index_name[1], '.png'), 
           p, width = 7, height = 3.5, dpi = 300)
  }
}





###########################
#       Plot bias by crop
###########################

fit_on_dry_wheat_Athena <- function(data) {
  # Find the minimum RWC in this subset
  min_rwc_data <- data %>%
    filter((crop == "Wheat Norwest Duet") & (RWC == min(RWC)) & soil == "Athena")
  
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



# Filter for fresh crops
fresh_df <- df %>% dplyr::filter(age == "fresh")

fresh_df_Athena <- fresh_df %>% 
  dplyr::filter(soil == "Athena")
# Fit on driest wheat on Athena
results <- fresh_df_Athena %>%
  group_by(index_name) %>%
  group_modify(~ fit_on_dry_wheat_Athena(.x))


results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ae <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)


# Perform min-max normalization for each index_name group
results_fr <- results_fr %>%
  group_by(index_name) %>%
  mutate(index_normalized = (index - min(index)) / (max(index) - min(index)))


# # Create the index_range column based on index_normalized values
# results_fr <- results_fr %>%
#   mutate(index_range_3groups = cut(index_normalized,
#                                    breaks = c(0, 0.15, 0.3, 1),
#                                    labels = c("0-0.15", "0.15-0.3", "0.3-1"),
#                                    include.lowest = TRUE))

# # Create the index_range column based on index_normalized values
# results_fr <- results_fr %>%
#   mutate(index_range_4groups = cut(index_normalized,
#                            breaks = c(0, 0.25, 0.5, 0.75, 1),
#                            labels = c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1"),
#                            include.lowest = TRUE))

# Create the fr_range column
results_fr <- results_fr %>%
  mutate(fr_range_4groups = cut(act_fr,
                           breaks = c(0, 0.15, 0.3, 0.75, 1),
                           labels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1"),
                           include.lowest = TRUE))

fr_table_4groups <- results_fr %>% 
  group_by(index_name, RWC, crop, fr_range_4groups) %>% 
  summarise(mae = round(mean(fr_ae), 2), 
            normalized_index_range = paste0(round(min(index_normalized), 2), "-", round(max(index_normalized), 2)))

view(fr_table_4groups)


error_summary <- results_fr %>%
  group_by(index_name, soil, RWC, crop) %>%
  summarise(
    mae = mean(fr_ae)
  )

df_to_plot <- error_summary
df_to_plot$RWC <- as.character(df_to_plot$RWC)

custom_colors <- c("Canola" = "#63ccb7", "Garbanzo Beans" = "#06728e", "Peas" = "#ff5643", 
                   "Wheat Norwest Duet" = "#06293c", "Wheat Pritchett" = "#8a5a44")  

for (sl in unique(df_to_plot$soil)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    p <- ggplot(filtered, aes(x = RWC, y = mae, color = crop, group = crop)) +
      geom_point(size = 2, alpha = 0.7) +       # Plot all points
      geom_line(size = 1, alpha = 0.7) +        # Add lines connecting points for the same crop
      scale_color_manual(name = "Crop residue", values = custom_colors) +
      labs(x = "RWC", y = "MAE") +
      theme_minimal() +   
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size ), # Legend text at base size
            axis.title = element_text(size = base_size ), # Axis titles larger than base size
            axis.text = element_text(size = base_size , color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, size = 14),
            strip.text = element_blank()) +
      ggtitle(filtered$index_name[1]) +
      coord_fixed(ratio = 7) 
    ggsave(paste0(path_to_plots, 'MAE/by_crop/', sl, "_", filtered$index_name[1], '.png'), 
           p, width = 7, height = 7, dpi = 300)
  }
}




###########################
#       Plot Error by soil
###########################
# Create the index_range column based on index_normalized values
results_fr <- results_fr %>%
  mutate(index_range_10groups = cut(index_normalized,
                                   breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                                   labels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"),
                                   include.lowest = TRUE))

fr_table_10groups_bySoil <- results_fr %>% 
  group_by(index_name, RWC, crop, index_range_10groups) %>% 
  summarise(fr_range = paste0(min(round(pred_fr, 2)), "-", max(round(pred_fr, 2))))

error_summary <- results_fr %>%
  group_by(index_name, crop, RWC, soil) %>%
  summarise(
    mae = mean(fr_ae)
  )

df_to_plot <- error_summary
df_to_plot$RWC <- as.character(df_to_plot$RWC)

custom_colors <- c("Athena" = "#582f0e", "Bagdad" = "#7f4f24", "Benwy"= "#936639",
                   "Broadax"= "#a68a64", "Endicott"= "#b6ad90", "Lance"= "#c2c5aa",
                   "Mondovi 1" = "#a4ac86", "Mondovi 2" = "#656d4a", "Oxy" = "#414833",
                   "Palouse" = "#333d29", "Ritzville" = "#774936", "Shano" =  "#580c1f")  

for  (crp in unique(df_to_plot$crop)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    
    filtered <- df_to_plot %>%
      filter(crop == crp, index_name == idx_)
    p <- ggplot(filtered, aes(x = RWC, y = mae, color = soil, group = soil)) +
      geom_point(size = 2, alpha = 0.7) +       # Plot all points
      geom_line(size = 1, alpha = 0.7) +        # Add lines connecting points for the same crop
      scale_color_manual(name = "Soil", values = custom_colors) +
      labs(x = "RWC", y = "MAE") +
      theme_minimal() +   
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size ), # Legend text at base size
            axis.title = element_text(size = base_size ), # Axis titles larger than base size
            axis.text = element_text(size = base_size , color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, size = 14),
            strip.text = element_blank()) +
      ggtitle(filtered$index_name[1]) +
      coord_fixed(ratio = 7) 
    ggsave(paste0(path_to_plots, 'MAE/by_soil/', crp, "_", filtered$index_name[1], '.png'), 
           p, width = 7, height = 7, dpi = 300)
  }
}


###########################
#       Plot Error by age
###########################

fit_on_dry_fresh <- function(data) {
  # Find the minimum RWC in this subset
  min_rwc_data <- data %>% filter(!str_detect(crop, "Weathered") & (RWC == min(RWC)))
  
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

# crops <- c("Canola", "Weathered Canola", "Wheat Norwest Duet", "Weathered Wheat")
canola <- c("Canola", "Weathered Canola")
wheat <- c("Wheat Norwest Duet", "Weathered Wheat")

# fresh_aged <- df %>% dplyr::filter(crop %in% crops)
fresh_aged_wheat <- df %>% dplyr::filter(crop %in% wheat)
fresh_aged_canola <- df %>% dplyr::filter(crop %in% canola)

# fresh_aged <- fresh_aged %>% dplyr::filter(soil == "Athena")
fresh_aged_wheat <- fresh_aged_wheat %>% dplyr::filter(soil == "Athena")
fresh_aged_canola <- fresh_aged_canola %>% dplyr::filter(soil == "Athena")

# # Fit on driest 
# results <- fresh_aged %>%
#   group_by(index_name, soil) %>%
#   group_modify(~ fit_on_dry_wheat(.x))

# Fit on driest 
results_wheat <- fresh_aged_wheat %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry_fresh(.x))

# Fit on driest 
results_canola <- fresh_aged_canola %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry_fresh(.x))



results <- results_canola
results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ae <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)

# Perform min-max normalization for each index_name group
results_fr <- results_fr %>%
  group_by(index_name) %>%
  mutate(index_normalized = (index - min(index)) / (max(index) - min(index)))

results_fr <- results_fr %>%
  mutate(index_range_3groups = cut(index_normalized,
                                   breaks = c(0, 0.15, 0.3, 1),
                                   labels = c("0-0.15", "0.15-0.3", "0.3-1"),
                                   include.lowest = TRUE))

fr_table_3groups_byAge <- results_fr %>% 
  group_by(index_name, soil, RWC, crop, index_range_3groups) %>% 
  summarise(mae = round(mean(fr_ae), 3))

error_summary <- results_fr %>%
  group_by(index_name, soil, RWC, crop) %>%
  summarise(
    mae = mean(fr_ae)
  )


custom_colors <- c(
  "Canola" = "#fe7f2d", "Weathered Canola" = "#fe7f2d",
  "Wheat Norwest Duet" = "#619b8a", "Weathered Wheat" = "#619b8a"
  )  

# Define the shapes: default shapes for normal crops, different shapes for weathered crops
custom_shapes <- c("Canola" = 16, "Weathered Canola" = 17, 
                   "Wheat Norwest Duet" = 16, "Weathered Wheat" = 17)

df_to_plot <- error_summary
for (sl in unique(df_to_plot$soil)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    p <- ggplot(filtered, aes(x = RWC, y = mae, color = as.factor(crop), shape = as.factor(crop))) +
      geom_point(size = 2, alpha = 0.7) +       # Plot all points
      geom_line(size = 1, alpha = 0.7) +        # Add lines connecting points for the same crop
      scale_color_manual(name = "Crop residue", values = custom_colors) +
      scale_shape_manual(name = "Crop residue", values = custom_shapes) +
      
      
      labs(x = "RWC", y = "MAE") +
      theme_minimal() +   
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size ), # Legend text at base size
            axis.title = element_text(size = base_size ), # Axis titles larger than base size
            axis.text = element_text(size = base_size , color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, size = 14),
            strip.text = element_blank()) +
      ggtitle(filtered$index_name[1]) +
      coord_fixed(ratio = 2) 
    ggsave(paste0(path_to_plots, 'MAE/by_age/', sl, "_", filtered$index_name[1], '.png'), 
           p, width = 15, height = 7, dpi = 300)
  }
}


####################################
####################################
################## Error by fr class

# Assuming your dataframe is named df
results_fr <- results_fr %>%
  mutate(fr_class = case_when(
    Fraction_Residue_Cover >= 0 & Fraction_Residue_Cover <= 0.15 ~ "0-15%",
    Fraction_Residue_Cover > 0.15 & Fraction_Residue_Cover <= 0.3 ~ "16-30%",
    Fraction_Residue_Cover > 0.3 ~ ">30%"
  ))


# df_to_plot <- results_fr %>% 
#   group_by(soil, index_name, RWC)

df_to_plot <- results_fr %>% dplyr::filter((index_name == "NDTI" & soil == "Athena"))

# Ensure fr_class is a factor and set the desired order
df_to_plot <- df_to_plot %>%
  mutate(fr_class = factor(fr_class, levels = c("0-15%", "16-30%", ">30%")))


# Assuming df is your dataframe and the bins for 'index' are defined using cut()
df_to_plot <- df_to_plot %>%
  mutate(index_bin = cut(round(index, 1), breaks = 5)) # Adjust 'breaks' to define the number of bins for index



# Adjusted plot: grouped boxplots
ggplot(df_to_plot, aes(x = index_bin, y = fr_ae, fill = fr_class, colour = fr_class)) +
  geom_boxplot(lwd = 0.5, outlier.size = 1, outlier.shape = 16) +  # Regular boxplot (no stacking)
  scale_fill_manual(values = c("0-15%" = "red", "16-30%" = "green", ">30%" = "blue")) +  # Customize colors
  scale_colour_manual(values = c("0-15%" = "red", "16-30%" = "green", ">30%" = "blue")) +  # Customize outlier colors
  labs(x = "Index Bins", y = "Fr absolute error", fill = "") +  # Labels
  theme_minimal(base_size = 15) +  # Adjust theme
  theme(legend.position = "right",
        legend.title = element_text(size = base_size), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        axis.title = element_text(size = base_size ), # Axis titles larger than base size
        axis.text = element_text(size = 7 , color = "black"), # Axis text at base size
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5, size = 14))+
  facet_wrap(~ RWC, scales = "free_y", labeller = labeller(RWC = function(x) paste("RWC =", x))) +
  guides(colour = "none")



####################################
####################################
############# 

# Loop over each unique RWC value and generate the corresponding plot
unique_rwc_values <- unique(df_to_plot$RWC)

for (rwc_value in unique_rwc_values) {
  
  # Filter the dataframe for the current RWC level
  df_filtered <- df_to_plot %>% dplyr::filter(RWC == rwc_value)
  
  # Adjusted plot: grouped boxplots with data points colored by 'crop'
  p <- ggplot(df_filtered, aes(x = index_bin, y = fr_ae, fill = fr_class, group = interaction(index_bin, fr_class))) +
    geom_boxplot(lwd = 0.5, outlier.size = 1, outlier.shape = 16,
                 position = position_dodge(width = 0.8)) +  # Regular boxplot
    geom_jitter(aes(color = crop), size = 3,
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +  # Jittered points
    scale_fill_manual(values = c("0-15%" = "red", "16-30%" = "green", ">30%" = "blue")) +  # Customize fill colors
    scale_color_manual(values = c("Canola" = "orange", "Garbanzo Beans" = "purple", "Peas" = "brown", "Wheat Norwest Duet" = "pink")) +  # Customize point colors
    labs(x = "Index Bins", y = "Fr absolute error", fill = "Fr class", color = "Crop residue") +  # Labels
    theme_minimal(base_size = 15) +  # Adjust theme
    theme(legend.position = "right",
          legend.title = element_text(size = 15),  # Legend title size
          legend.text = element_text(size = 15),   # Legend text size
          axis.title = element_text(size = 15),    # Axis title size
          axis.text = element_text(size = 16, color = "black"),  # Axis text size
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),  # White background
          plot.background = element_rect(fill = "white", colour = "white"),
          panel.grid = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black"),
          legend.key.size = unit(1, "cm"),
          plot.title = element_text(hjust = 0.5, size = 14))
  
  # Define the save location and filename
  file_name <- paste0(path_to_plots, 'MAE/stackedbox_by_crop/', "_", df_filtered$index_name[1], '_RWC_', rwc_value, '.png')
  
  # Save the plot
  ggsave(file_name, p, width = 15, height = 7, dpi = 300)
  
}







################################################
################################################
############ 
############   statistical test
############

 
df_to_plot$crop <- factor(df_to_plot$crop)

df_to_plot$crop <- relevel(df_to_plot$crop, ref = "Wheat Norwest Duet")

df_to_plot_ <- df_to_plot %>% dplyr::filter(RWC == 0)
  
# Fit the linear model with interaction
model_interaction <- lm(Fraction_Residue_Cover ~ index * crop, data = df_to_plot_)


# Fit the reduced model (without interaction)
model_reduced <- lm(Fraction_Residue_Cover ~ index + crop, data = df_to_plot_)


# Compare the models using ANOVA
anova(model_reduced, model_interaction)


summary(model_interaction)

