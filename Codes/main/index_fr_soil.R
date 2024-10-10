library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)
library(broom)
library(ggpattern)
library(gridExtra)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')
path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/soil_type_comparison/')

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

# Add fresh/weathered column 
fresh <- c("Canola", "Garbanzo Beans", "Peas", "Wheat Norwest Duet", 
           "Wheat Pritchett")

weathered <- c("Weathered Canola", "Weathered Wheat")
df <- df %>% 
  mutate(age = case_when(
    crop %in% fresh ~ "fresh", 
    crop %in% weathered ~ "weathered"
  ))

# # Remove weathered wheat and weathered canola
# df <- df %>% dplyr::filter(!(crop == "Weathered Canola" & index_name == "NDTI"))
# df <- df %>% dplyr::filter(!(crop == "Weathered Canola" & index_name == "SINDRI"))
# df <- df %>% dplyr::filter(!(crop == "Weathered Canola" & index_name == "CAI"))
# 
# 
# df <- df %>% dplyr::filter(!(crop == "Weathered Wheat" & index_name == "NDTI"))
# df <- df %>% dplyr::filter(!(crop == "Weathered Wheat" & index_name == "SINDRI"))
# df <- df %>% dplyr::filter(!(crop == "Weathered Wheat" & index_name == "CAI"))


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

# Filter the DataFrame for specific RWC values
df <- df %>%
  filter(RWC %in% c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Fit on all crops for each index and each soil  
results3 <- df %>%
  group_by(index_name, crop) %>%
  group_modify(~ process_group(.x))




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
# write.csv(results3_fr, paste0(path_to_data, "for_Kirti/Dataset_for_soiltypeimpact.csv"), row.names = FALSE)

# Filter for one crop type: "Wheat Norwest Duet"
sl <- unique(results3$crop)[5]
unfiltered_df <- results3_fr # To include all soils
results3_fr <- results3_fr %>% dplyr::filter(crop == sl)



NDTI_df <- results3_fr %>% dplyr::filter(index_name == "NDTI")
CAI_df <- results3_fr %>% dplyr::filter(index_name == "CAI")
SINDRI_df <- results3_fr %>% dplyr::filter(index_name == "SINDRI")


# fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = fr_dif)) +
#   geom_boxplot(alpha = 0.6, outlier.size = 2) +  # Setting alpha directly for point transparency
#   labs(x = "actual fraction", y = "fr_difference", title = paste0(df_$index_name[1], "_across_crops")) +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +  # Setting a color palette for the crops
#   scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7)) +
#   theme(legend.position = "right",
#         legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
#         legend.text = element_text(size = base_size), # Legend text at base size
#         plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
#         axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
#         axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
#         panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
#         plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
#         panel.grid = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.line = element_line(color = "black"),
#         legend.key.size = unit(0.5, "cm")) 
#   # geom_hline(yintercept=1)
# print(fr_ratio_plot)
# ggsave(paste0(path_to_plots, 'FR_differene_fr_act_box/', 'fr_fitted_box_', df_$index_name[1], '.png'), fr_ratio_plot, width = 10, height = 7, dpi = 300)


df_ <- NDTI_df

# ####################################
# # Creating the custom labeller function
# custom_labeller <- function(variable, value) {
#   return(paste("RWC =", value))
# }
# 
# ### Fr_difference facet across RWCs
# base_size <- 14
# # Creating the plot
# fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = fr_dif, color = factor(RWC))) +
#   geom_boxplot(alpha = 0.6, outlier.size = 2) +  # Setting alpha directly for point transparency
#   labs(x = "Actual Fraction", y = expression(f[r] * " difference"), 
#        title = paste0(df_$index_name[1], "_", df_$crop[1]), color = "RWC") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +  # Setting a color palette
#   theme(legend.position = "right",
#         legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
#         legend.text = element_text(size = base_size), # Legend text at base size
#         plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
#         axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
#         axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
#         panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
#         plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
#         panel.grid = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.line = element_line(color = "black"),
#         legend.key.size = unit(0.5, "cm"),
#         strip.text = element_text(size = base_size * 1.1)) + # Adjusting facet title size
#   facet_wrap(~RWC, scales = "free", labeller = custom_labeller) 
# 
# print(fr_ratio_plot)
# 
# ggsave(paste0(path_to_plots, 'fr_differene_range_all_soils/', df_$index_name[1], '.png'), fr_ratio_plot, width = 15, height = 7, dpi = 300)
# 
# 
# 
# # Plot for all soils
# fr_ratio_plot_allsoils <- ggplot(unfiltered_df, aes(x = factor(act_fr), y = fr_dif, color = factor(RWC))) +
#   geom_boxplot(alpha = 0.6, outlier.size = 2) +  # Setting alpha directly for point transparency
#   labs(x = "Actual Fraction", y = expression(f[r] * " difference"), 
#        title = paste0(df_$index_name[1], "_all_crops"), color = "RWC") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +  # Setting a color palette
#   theme(legend.position = "right",
#         legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
#         legend.text = element_text(size = base_size), # Legend text at base size
#         plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
#         axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
#         axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
#         panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
#         plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
#         panel.grid = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.line = element_line(color = "black"),
#         legend.key.size = unit(0.5, "cm"),
#         strip.text = element_text(size = base_size * 1.1)) + # Adjusting facet title size
#   facet_wrap(~RWC, scales = "free", labeller = custom_labeller) 
# 
# combined_plot <- grid.arrange(fr_ratio_plot, fr_ratio_plot_allsoils, ncol = 1)
# 
# print(combined_plot)
# ggsave(paste0(path_to_plots, 'fr_differene_range_all_crops/', df_$index_name[1], '.png'), combined_plot, width = 15, height = 10, dpi = 300)
# 
# # Fresh/weathered separated
# fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = fr_dif, fill = factor(age))) +
#   geom_boxplot_pattern(alpha = 0.6, outlier.size = 2, color = "black", pattern_angle = 45) +  # Setting alpha directly for point transparency
#   scale_fill_brewer(palette = "Set1") +  # Use a discrete color palette for fills
#   labs(x = "Actual Fraction", y = expression(f[r] * " difference"), 
#        title = paste0(df_$index_name[1]), color = "RWC", fill = "Age") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +  # Setting a color palette
#   theme(legend.position = "right",
#         legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
#         legend.text = element_text(size = base_size), # Legend text at base size
#         plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
#         axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
#         axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
#         panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
#         plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
#         panel.grid = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.line = element_line(color = "black"),
#         legend.key.size = unit(0.5, "cm"),
#         strip.text = element_text(size = base_size * 1.1)) + # Adjusting facet title size
#   # scale_pattern_manual(values = c("crosshatch", "dots")) +
#   facet_wrap(~RWC, scales = "free", labeller = custom_labeller) 
# 
# print(fr_ratio_plot)
# 
# ggsave(paste0(path_to_plots, 'fr_differene_by_age/', df_$index_name[1], '.png'), fr_ratio_plot, width = 15, height = 7, dpi = 300)
# 
# ####################################
# ### Fr_prediction range facet across RWCs
# 
# base_size <- 14
# # Calculate min and max per group
# summary_df <- df_ %>%
#   group_by(act_fr, RWC) %>%
#   summarise(
#     min_pred_fr = round(min(pred_fr), 2),
#     max_pred_fr = round(max(pred_fr), 2),
#     .groups = 'drop'
#   )
# 
# # Creating the plot
# fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = pred_fr, color = factor(RWC))) +
#   geom_boxplot(alpha = 0.6, outlier.size = 2) +
#   geom_text(data = summary_df, aes(label = format(min_pred_fr, digits = 3), y = min_pred_fr), 
#             vjust = 1.1, color = "blue", position = position_dodge(width = 0.75)) +
#   geom_text(data = summary_df, aes(label = format(max_pred_fr, digits = 3), y = max_pred_fr), 
#             vjust = -0.3, color = "red", position = position_dodge(width = 0.75)) +
#   labs(x = "Actual Fraction", y = expression("Predicted " * f[r]), 
#        title = paste0(df_$index_name[1], "_fresh"), color = "RWC") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +
#   theme(legend.position = "right",
#         legend.title = element_text(size = base_size * 1.2),
#         legend.text = element_text(size = base_size),
#         plot.title = element_text(size = base_size * 1.5, hjust = 0.5),
#         axis.title = element_text(size = base_size * 1.2),
#         axis.text = element_text(size = base_size, color = "black"),
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white"),
#         panel.grid = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.line = element_line(color = "black"),
#         legend.key.size = unit(0.5, "cm"),
#         strip.text = element_text(size = base_size * 1.1)) +
#   facet_wrap(~RWC, scales = "free", labeller = custom_labeller)
# 
# print(fr_ratio_plot)
# ggsave(paste0(path_to_plots, 'fr_pred_range_all_crops/',
#               df_$index_name[1], '.png'), fr_ratio_plot, width = 15, height = 10,
#        dpi = 300)
# 
# ### Fr_prediction range facet across RWCs separated by age
# summary_df <- df_ %>%
#   group_by(act_fr, RWC, age) %>%
#   summarise(
#     min_pred_fr = round(min(pred_fr), 2),
#     max_pred_fr = round(max(pred_fr), 2),
#     .groups = 'drop'
#   )
# 
# fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = pred_fr, fill = factor(age))) +
#   geom_boxplot_pattern(alpha = 0.6, outlier.size = 2, color = "black", pattern_angle = 45) +
#   geom_text(data = summary_df, aes(x = factor(act_fr), label = format(min_pred_fr, digits = 3), y = min_pred_fr), 
#             vjust = 1.1, color = "blue", position = position_dodge(width = 0.75)) +
#   geom_text(data = summary_df, aes(x = factor(act_fr), label = format(max_pred_fr, digits = 3), y = max_pred_fr), 
#             vjust = -0.3, color = "red", position = position_dodge(width = 0.75)) +
#   labs(x = "Actual Fraction", y = expression("Predicted " * f[r]), 
#        title = paste0(df_$index_name[1]), color = "RWC", fill = "Age") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +
#   theme(legend.position = "right",
#         legend.title = element_text(size = base_size * 1.2),
#         legend.text = element_text(size = base_size),
#         plot.title = element_text(size = base_size * 1.5, hjust = 0.5),
#         axis.title = element_text(size = base_size * 1.2),
#         axis.text = element_text(size = base_size, color = "black"),
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white"),
#         panel.grid = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.line = element_line(color = "black"),
#         legend.key.size = unit(0.5, "cm"),
#         strip.text = element_text(size = base_size * 1.1)) +
#   facet_wrap(~RWC, scales = "free", labeller = custom_labeller)
# 
# print(fr_ratio_plot)
# 
# ggsave(paste0(path_to_plots, 'fr_pred_range_by_age_all_crops/',
#               df_$index_name[1], '.png'), fr_ratio_plot, width = 21, height = 13,
#        dpi = 300)

#######################################
#######################################
#######################################
#######################################
#           Plot Fr ~ index 
#######################################
#######################################


########## Dry fits
# fr_idx_df <- unfiltered_df
# fr_idx_df <- fr_idx_df %>% dplyr::filter(RWC == 0)
# base_size = 14
# # sl <- unique(fr_idx_df$soil)[1]
# for (sl in unique(fr_idx_df$soil)){
#   for (idx_ in unique(fr_idx_df$index_name)){
#     filtered <- fr_idx_df %>% dplyr::filter(soil == sl)
#     filtered <- filtered %>% dplyr::filter(index_name == idx_)
#     
#     p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(RWC), shape = as.factor(crop))) +
#       geom_point() + # Adds points to the plot
#       geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
#       facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
#       scale_color_discrete(name = "RWC") +
#       scale_shape_discrete(name = "Crop") + # Adds a legend for RWC
#       labs(x = "Index", y = "Fraction Residue Cover") + # Labels for axes
#       theme_minimal() + # Minimal theme for cleaner look
#       theme(legend.position = "right",
#             legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
#             legend.text = element_text(size = base_size), # Legend text at base size
#             plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
#             axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
#             axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
#             panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
#             plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
#             panel.grid = element_blank(),
#             axis.ticks = element_line(color = "black"),
#             axis.line = element_line(color = "black"),
#             legend.key.size = unit(0.5, "cm")) 
#     
#     # Print the plot
#     print(p)
#     ggsave(paste0(path_to_plots, 'fr_index_dry_fits/', sl, "_", filtered$index_name[1], '.png'), p, width = 10, height = 7, dpi = 300)
#   }
# }



#######################################
#######################################
#######################################
#######################################

######### Across crops and RWCs
custom_labeller <- function(variable, value) {
  return(paste("RWC =", value))
}
base_size = 14
# Creating the scatter plot
fr_ratio_plot <- ggplot(df_, aes(x = pred_fr, y = act_fr, color = factor(RWC), shape = factor(soil))) +
  geom_point(alpha = 0.6, size = 2) +  # Plot points with some transparency
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a 1:1 line in red
  labs(x = "Predicted Fraction", y = "Actual Fraction",
       title = paste0(df_$index_name[1], " Comparison of Predicted vs. Actual Fraction"),
       color = "RWC", shape = "Crop") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette for RWC
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13)) +  # Define shapes for each crop type
  scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.2)) +  # Set x-axis from 0 to 1
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.2)) +  # Set y-axis from 0 to 1
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
    legend.key.size = unit(0.5, "cm"),
    strip.text = element_text(size = base_size * 1.1)
  ) +
  facet_wrap(~RWC, scales = "free", labeller = custom_labeller)  # Facet by RWC

print(fr_ratio_plot)
# ggsave(paste0(path_to_plots, 'fr_pred_act_fits_/', sl, "_", df_$index_name[1], '.png'), fr_ratio_plot, width = 13, height = 7, dpi = 300)

