library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)
library(broom)
library(gridExtra)


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
                   "Wheat Norwest Duet" = "#619b8a", "Wheat Pritchett" = "#a1c181")  


for (sl in unique(df_to_plot$soil)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes, by = "crop") %>%
      mutate(crop_label = paste0(crop, " (", round(slope_value, 2), ")"))
    
    # Print filtered data after the join to check if slope_value has been joined correctly
    print("Filtered Data after left_join:")
    print(head(filtered))
    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop))) +
      geom_point(size = 4) + # Adds points to the plot
      geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Crop residue (slope)", values = custom_colors,
                         labels = local_crop_labels) +
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
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/fresh/', sl, "_", filtered$index_name[1], '.png'), 
           p, width = 10, height = 7, dpi = 300)
  }
}



###########################
# Plot Fresh vs weathered canola and wheat
###########################

crops <- c("Canola", "Weathered Canola", "Wheat Norwest Duet", "Weathered Wheat")
df_to_plot <- dry_df %>% dplyr::filter(crop %in% crops)
# Specify the desired order of crops for the legend
desired_order <- c("Canola", "Weathered Canola", "Wheat Norwest Duet", "Weathered Wheat")

# Update df_to_plot to set the order of the factor levels for 'crop'
df_to_plot <- df_to_plot %>%
  mutate(crop = factor(crop, levels = desired_order))

custom_colors <- c("Canola" = "#fe7f2d", "Weathered Canola" = "#fe7f2d",
                   "Wheat Norwest Duet" = "#619b8a", "Weathered Wheat" = "#619b8a"
                   )  
# Define the shapes: default shapes for normal crops, different shapes for weathered crops
custom_shapes <- c("Canola" = 16, "Weathered Canola" = 17, 
                   "Wheat Norwest Duet" = 16, "Weathered Wheat" = 17)

for (sl in unique(df_to_plot$soil)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes, by = "crop") %>%
      mutate(crop_label = paste0(crop, " (", round(slope_value, 2), ")"))
    
    # Print filtered data after the join to check if slope_value has been joined correctly
    print("Filtered Data after left_join:")
    print(head(filtered))
    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop), shape = as.factor(crop))) +
      geom_point(size = 4) + # Adds points to the plot
      geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Crop residue (slope)", values = custom_colors,
                         labels = local_crop_labels) +
      scale_shape_manual(name = "Crop residue (slope)", values = custom_shapes, labels = local_crop_labels) +
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
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/by_age/', sl, "_", filtered$index_name[1], '.png'), 
           p, width = 10, height = 7, dpi = 300)
  }
}


###########################
# Plot fr ~ index across soils
###########################

custom_colors <- c("Athena" = "#582f0e", "Bagdad" = "#7f4f24", "Benwy"= "#936639",
                   "Broadax"= "#a68a64", "Endicott"= "#b6ad90", "Lance"= "#c2c5aa",
                   "Mondovi 1" = "#a4ac86", "Mondovi 2" = "#656d4a", "Oxy" = "#414833",
                   "Palouse" = "#333d29", "Ritzville" = "#774936", "Shano" =  "#580c1f")  


for (crp in unique(df_to_plot$crop)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(crop == crp, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes <- filtered %>%
      group_by(soil) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes, by = "soil") %>%
      mutate(soil_label = paste0(soil, " (", round(slope_value, 2), ")"))
    
    # Print filtered data after the join to check if slope_value has been joined correctly
    print("Filtered Data after left_join:")
    print(head(filtered))
    
    # Dynamically create the labels for the legend
    local_soil_labels <- filtered %>%
      distinct(soil, soil_label) %>%
      pull(soil_label, soil)
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(soil))) +
      geom_point(size = 4) + # Adds points to the plot
      geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Soil (slope)", values = custom_colors,
                         labels = local_soil_labels) +
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
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/soil/', crp, "_", filtered$index_name[1], '.png'), 
           p, width = 10, height = 7, dpi = 300)
  }
}





###########################
#       Plot Error by crop
###########################

# Filter for fresh crops
fresh_df <- df %>% dplyr::filter(age == "fresh")

# Fit on driest 
results <- fresh_df %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry(.x))


results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ratio_ <- results_fr$Predicted_Fraction_Residue_Cover/results_fr$Fraction_Residue_Cover
results_fr$fr_dif <- results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover
results_fr$fr_ae <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)

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

crops <- c("Canola", "Weathered Canola", "Wheat Norwest Duet", "Weathered Wheat")
fresh_aged <- df %>% dplyr::filter(crop %in% crops)

# fresh_aged <- fresh_aged %>% dplyr::filter(soil == "Athena")

# Fit on driest 
results <- fresh_aged %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry(.x))


results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ratio_ <- results_fr$Predicted_Fraction_Residue_Cover/results_fr$Fraction_Residue_Cover
results_fr$fr_dif <- results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover
results_fr$fr_ae <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)

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



