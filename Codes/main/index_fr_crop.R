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


fit_on_all <- function(data) {
  
  # Fit linear model to the data
  model <- lm(Fraction_Residue_Cover ~ index, data = data)
  
  # Predict Fraction_Residue_Cover for the entire group's data
  predictions <- predict(model, newdata = data)
  
  # Add predictions to the data
  data$Predicted_Fraction_Residue_Cover <- predictions
  
  # Calculate predictions for the minimum RWC data only
  predictions <- predict(model, newdata = data)
  
  # Add slope and intercept from the model
  data$Slope <- coef(model)[["index"]]
  data$Intercept <- coef(model)[["(Intercept)"]]
  
  return(data)
}

# Fit on driest 
results <- df %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry(.x))

# Fit on each crops for each index and each soil  
results2 <- df %>%
  group_by(index_name, soil, crop, RWC) %>%
  group_modify(~ fit_on_all(.x))

# Fit on all crops 
results3 <- df %>%
  group_by(index_name, soil, RWC) %>%
  group_modify(~ fit_on_all(.x))

# Filter the DataFrame for specific RWC values
results <- results %>%
  filter(RWC %in% c(0, 0.2, 0.4, 0.6, 0.8, 1))
results2 <- results2 %>%
  filter(RWC %in% c(0, 0.2, 0.4, 0.6, 0.8, 1))
results3 <- results3 %>%
  filter(RWC %in% c(0, 0.2, 0.4, 0.6, 0.8, 1))


##############################
        # fr_differene
##############################
results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ratio_ <- results_fr$Predicted_Fraction_Residue_Cover/results_fr$Fraction_Residue_Cover
results_fr$fr_dif <- results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover
# write.csv(results_fr, paste0(path_to_data, "for_Kirti/Dataset.csv"), row.names = FALSE)


results_fr2 = results2
results_fr2$act_fr <- results_fr2$Fraction_Residue_Cover
results_fr2$pred_fr <- results_fr2$Predicted_Fraction_Residue_Cover
results_fr2$fr_ratio_ <- results_fr2$Predicted_Fraction_Residue_Cover/results_fr2$Fraction_Residue_Cover
results_fr2$fr_dif <- results_fr2$Predicted_Fraction_Residue_Cover - results_fr2$Fraction_Residue_Cover



results_fr3 = results3
results_fr3$act_fr <- results_fr3$Fraction_Residue_Cover
results_fr3$pred_fr <- results_fr3$Predicted_Fraction_Residue_Cover
results_fr3$fr_ratio_ <- results_fr3$Predicted_Fraction_Residue_Cover/results_fr3$Fraction_Residue_Cover
results_fr3$fr_dif <- results_fr3$Predicted_Fraction_Residue_Cover - results_fr3$Fraction_Residue_Cover


fr_summary <- results_fr3 %>% 
  group_by(index_name, soil, RWC, crop) %>% 
  summarise(
    mean_dif = mean(fr_dif),
    min_fr = min(pred_fr),
    max_fr = max(pred_fr)
  )
# write.csv(fr_summary, paste0(path_to_data, "for_Kirti/fr_summary.csv"), row.names = FALSE)



# Filter for fresh and weathered wheat 
# results_fr <- results_fr %>%
#   dplyr::filter(crop == "Wheat Norwest Duet" | crop == "Weathered Wheat")
# results_fr2 <- results_fr2 %>%
#   dplyr::filter(crop == "Wheat Norwest Duet" | crop =="Weathered Wheat")

# results_fr <- results_fr %>%
#   dplyr::filter(crop == "Wheat Pritchett" | crop == "Weathered Wheat")
# results_fr2 <- results_fr2 %>%
#   dplyr::filter(crop == "Wheat Pritchett" | crop =="Weathered Wheat")

# results_fr <- results_fr %>%
#   dplyr::filter(crop == "Canola" | crop == "Weathered Canola")
# results_fr2 <- results_fr2 %>%
#   dplyr::filter(crop == "Canola" | crop =="Weathered Canola")

#######################################
#######################################
#           Analysis
#######################################
#######################################
#### fr pred CV
results_fr <- results_fr %>%
  mutate(MAE = abs(pred_fr - act_fr),
         RMSE = (pred_fr - act_fr)^2,
         MAPE = (sum((abs(pred_fr - act_fr) / pred_fr)) * 100)/7
             )


# dry_fr <- results_fr %>% dplyr::filter(RWC == 0)

results_fr <- results_fr %>% 
  mutate(age = case_when(
    crop %in% fresh ~ "fresh", 
    crop %in% weathered ~ "weathered"
  ))


age_summary <- results_fr %>% 
  group_by(index_name, age,RWC) %>% 
  summarise(
    min_MAE = min(MAE),
    max_MAE = max(MAE),
    mean_RMSE = mean(RMSE), 
    mean_MAPe = mean(MAPE)
  )

all_summary <- results_fr %>% 
  group_by(index_name, RWC, crop) %>% 
  summarise(
    mean_MAE = mean(MAE), 
    mean_RMSE = mean(RMSE), 
    mean_MAPe = mean(MAPE)
  )


all_summary <- all_summary %>% 
  mutate(age = case_when(
    crop %in% fresh ~ "fresh", 
    crop %in% weathered ~ "weathered"
  ))

all_summary <- all_summary %>% 
  group_by(index_name, age) %>% 
  summarise(mean_across_RWC = mean(mean_MAE))

################################################
################################################
################################################


dry_summary2 <- all_summary %>% 
  group_by(index_name) %>%
  summarise(mean_MAE_all_fr = mean(mean_MAE))

  



base_size = 14
plot <- ggplot(age_summary, aes(x = as.factor(act_fr), y = mean_MAE)) +
  geom_boxplot(alpha = 0.6, size = 0.5) +  # Plot points with some transparency
  # geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a 1:1 line in red
  labs(x = "Actual fr", y = "MAE of predicted fr",
       title = paste0("")
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette for RWC
  # scale_shape_manual(values = c(0, 1, 2, 3, 4, 6, 8)) +  # Define shapes for each crop type
  # scale_x_continuous(limits = c(0, 1.4), breaks = seq(0, 1.4, 0.2)) +  # Set x-axis from 0 to 1
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
    legend.key.size = unit(0.5, "cm"),
    strip.text = element_text(size = base_size * 1.1),
  ) +
  facet_grid(index_name ~ crop, scales = "free")


print(plot)

################################################
################################################
################################################












cv_table <- summary %>% 
  group_by(index_name, RWC) %>% 
  summarise(
    CV_range = paste0(round(min(cv), 2), "-", round(max(cv), 2))
  )

# Define your custom labeller function
custom_labeller <- function(variable, value) {
  if (variable == "RWC") {
    return(paste("RWC = ", value))
  } else {
    return(as.character(value))
  }
}

# plot CVs
base_size <- 14
# Creating the scatter plot across soils

df_plot <- summary
plot <- ggplot(df_plot, aes(x = as.factor(act_fr), y = cv)) +
  geom_boxplot(alpha = 0.6, size = 0.5) +  # Plot points with some transparency
  # geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a 1:1 line in red
  stat_summary(
    fun = min, geom = "text", aes(label = round(..y.., 1)),
    vjust = 1.5, size = 3, color = "blue"
  ) +
  stat_summary(
    fun = max, geom = "text", aes(label = round(..y.., 1)),
    vjust = -0.5, size = 3, color = "red"
  ) +
  labs(x = "Actual fr", y = "CV of predicted fr",
       title = paste0("")
       ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette for RWC
  # scale_shape_manual(values = c(0, 1, 2, 3, 4, 6, 8)) +  # Define shapes for each crop type
  # scale_x_continuous(limits = c(0, 1.4), breaks = seq(0, 1.4, 0.2)) +  # Set x-axis from 0 to 1
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 30)) +  # Set y-axis from 0 to 1
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
    strip.text = element_text(size = base_size * 1.1),
  ) +
  facet_grid(index_name ~ RWC, scales = "free", labeller = custom_labeller)

print(plot)
# ggsave(paste0(path_to_plots, 'fr_cv/', 'fr_cv_crop.png'), plot, width = 20, height = 10, dpi = 300)
# ggsave(paste0(path_to_plots, 'fr_cv/', 'fr_cv_', sl, '.png'), plot, width = 20, height = 10, dpi = 300)

# write.csv(results_fr2, paste0(path_to_data, "for_Kirti/slope_intcept_dataset.csv"), row.names = FALSE)
results_range <- results_fr2 %>%
  group_by(index_name, soil, crop, RWC) %>%
  summarise(
    Slope = Slope[1],
    Intercept = Intercept[1],
  )

mean_slp_intpt <- results_range %>% 
  group_by(index_name, RWC) %>% 
  summarise(
    mean_slope = mean(Slope), 
    mean_intercept = mean(Intercept)
  )
# write.csv(mean_slp_intpt, paste0(path_to_data, "for_Kirti/mean_slp_intpt.csv"), row.names = FALSE)

crop_slp_intpt <- results_range %>% 
  group_by(index_name, crop, RWC) %>% 
  summarise(
    min_slope = min(Slope), 
    max_slope = max(Slope),
    min_Intercept = min(Intercept), 
    max_Intercept = max(Intercept),
  )
crop_slp_intpt <- crop_slp_intpt %>% dplyr::filter(
  RWC == 0 | RWC == 1
)
# write.csv(crop_slp_intpt, paste0(path_to_data, "for_Kirti/crop_slp_intpt.csv"), row.names = FALSE)

results_range_by_age <- results_fr2 %>%
  group_by(index_name, RWC, age) %>%
  summarise(
    mean_slope = mean(Slope),
    mean_intercept = mean(Intercept),
  )
# write.csv(results_range_by_age, paste0(path_to_data, "for_Kirti/results_range_by_age_wheat.csv"), row.names = FALSE)

#######
# # # Plot fresh vs weathered mean slope
# NDTI <- results_range_by_age %>% dplyr::filter(index_name == "NDTI")
# CAI <- results_range_by_age %>% dplyr::filter(index_name == "CAI")
# SINDRI <- results_range_by_age %>% dplyr::filter(index_name == "SINDRI")
# 
# base_size = 14
# fr_ratio_plot <- ggplot(results_range_by_age, aes(x = RWC, y = mean_slope, color = factor(age))) +
#   geom_point(alpha = 0.6, size = 2) +
#   labs(x = "RWC", y = "Mean slope",
#        title = paste0("fresh vs weathered ", unique(results_fr2$crop)[1]),
#        color = "Age") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Set1") +  # Setting a color palette
#   scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +  # Set x-axis from 0 to 1
#   # scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +  # Set y-axis from 0 to 1
#   theme(
#     legend.position = "right",
#     legend.title = element_text(size = base_size * 1.2),  # Legend title larger than base size
#     legend.text = element_text(size = base_size),  # Legend text at base size
#     plot.title = element_text(size = base_size * 1.5, hjust = 0.5),  # Title larger than base size
#     axis.title = element_text(size = base_size * 1.2),  # Axis titles larger than base size
#     axis.text = element_text(size = base_size, color = "black"),  # Axis text at base size
#     panel.background = element_rect(fill = "white", colour = "white"),  # Set panel background to white
#     plot.background = element_rect(fill = "white", colour = "white"),  # Set plot background to white
#     panel.grid = element_blank(),
#     axis.ticks = element_line(color = "black"),
#     axis.line = element_line(color = "black"),
#     legend.key.size = unit(0.5, "cm"),
#     strip.text = element_text(size = base_size * 1.1)
#   ) +
#   facet_wrap(~index_name, scales = "free")
# 
# 
# print(fr_ratio_plot)
# ggsave(paste0(path_to_plots, 'by_age_appleToapple/', unique(results_fr2$crop)[1], '.png'), fr_ratio_plot, width = 15, height = 7, dpi = 300)
#######################################
#######################################
#######################################
#######################################


# Filter for one soil type
sl <- unique(results$soil)[1]  # 1: Athena
unfiltered_df <- results_fr # To include all soils
results_fr <- results_fr %>% dplyr::filter(soil == sl)


NDTI_df <- results_fr %>% dplyr::filter(index_name == "NDTI")
CAI_df <- results_fr %>% dplyr::filter(index_name == "CAI")
SINDRI_df <- results_fr %>% dplyr::filter(index_name == "SINDRI")


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

####################################
# Creating the custom labeller function
custom_labeller <- function(variable, value) {
  return(paste("RWC =", value))
}

### Fr_difference facet across RWCs
base_size <- 14
# Creating the plot
fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = fr_dif, color = factor(RWC))) +
  geom_boxplot(alpha = 0.6, outlier.size = 2) +  # Setting alpha directly for point transparency
  labs(x = "Actual Fraction", y = expression(f[r] * " difference"), 
       title = paste0(df_$index_name[1], "_", df_$soil[1]), color = "RWC") +
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
        legend.key.size = unit(0.5, "cm"),
        strip.text = element_text(size = base_size * 1.1)) + # Adjusting facet title size
  facet_wrap(~RWC, scales = "free", labeller = custom_labeller) 

print(fr_ratio_plot)

# ggsave(paste0(path_to_plots, 'fr_differene_range_all_crops/', df_$index_name[1], '.png'), fr_ratio_plot, width = 15, height = 7, dpi = 300)



# Plot for all soils
fr_ratio_plot_allsoils <- ggplot(unfiltered_df, aes(x = factor(act_fr), y = fr_dif, color = factor(RWC))) +
  geom_boxplot(alpha = 0.6, outlier.size = 2) +  # Setting alpha directly for point transparency
  labs(x = "Actual Fraction", y = expression(f[r] * " difference"), 
       title = paste0(df_$index_name[1], "_all_soils"), color = "RWC") +
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
        legend.key.size = unit(0.5, "cm"),
        strip.text = element_text(size = base_size * 1.1)) + # Adjusting facet title size
  facet_wrap(~RWC, scales = "free", labeller = custom_labeller) 

combined_plot <- grid.arrange(fr_ratio_plot, fr_ratio_plot_allsoils, ncol = 1)

print(combined_plot)
# ggsave(paste0(path_to_plots, 'fr_differene_range_all_soils/', df_$index_name[1], '.png'), combined_plot, width = 15, height = 10, dpi = 300)



# Filter for wheat and wheathered wheat
df_wheat <- df_ %>%
  dplyr::filter(crop == "Weathered Canola")
df_wheat <- df_wheat %>% 
  dplyr::filter(crop == "Canola")

# Fresh/weathered separated
fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = fr_dif, fill = factor(age))) +
  geom_boxplot_pattern(alpha = 0.6, outlier.size = 2, color = "black", pattern_angle = 45) +  # Setting alpha directly for point transparency
  scale_fill_brewer(palette = "Set1") +  # Use a discrete color palette for fills
  # labs(x = "Actual Fraction", y = expression(f[r] * " difference"),
  labs(x = "Actual Fraction", y = "Difference (Actual - Predicted) Fraction",
       title = paste0("Fresh vs Wheathered Canola"), color = "RWC", fill = "Age") +
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
        legend.key.size = unit(0.5, "cm"),
        strip.text = element_text(size = base_size * 1.1), # Adjusting facet title size
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # scale_pattern_manual(values = c("crosshatch", "dots")) +
  facet_wrap(~RWC, scales = "free", labeller = custom_labeller) 

print(fr_ratio_plot)

# ggsave(paste0(path_to_plots, 'fr_differene_by_age/', df_$index_name[1], '.png'), fr_ratio_plot, width = 15, height = 7, dpi = 300)

####################################
### Fr_prediction range facet across RWCs

base_size <- 14
# Calculate min and max per group
summary_df <- df_ %>%
  group_by(act_fr, RWC) %>%
  summarise(
    min_pred_fr = round(min(pred_fr), 2),
    max_pred_fr = round(max(pred_fr), 2),
    .groups = 'drop'
  )

# Creating the plot
fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = pred_fr, color = factor(RWC))) +
  geom_boxplot(alpha = 0.6, outlier.size = 2) +
  geom_text(data = summary_df, aes(label = format(min_pred_fr, digits = 3), y = min_pred_fr), 
            vjust = 1.1, color = "blue", position = position_dodge(width = 0.75)) +
  geom_text(data = summary_df, aes(label = format(max_pred_fr, digits = 3), y = max_pred_fr), 
            vjust = -0.3, color = "red", position = position_dodge(width = 0.75)) +
  labs(x = "Actual Fraction", y = expression("Predicted " * f[r]), 
       title = paste0(df_$index_name[1], "_fresh"), color = "RWC") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2),
        legend.text = element_text(size = base_size),
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5),
        axis.title = element_text(size = base_size * 1.2),
        axis.text = element_text(size = base_size, color = "black"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"),
        strip.text = element_text(size = base_size * 1.1)) +
  facet_wrap(~RWC, scales = "free", labeller = custom_labeller)

print(fr_ratio_plot)
# ggsave(paste0(path_to_plots, 'fr_pred_range_all_crops/',
#               df_$index_name[1], '.png'), fr_ratio_plot, width = 15, height = 10,
#        dpi = 300)

### Fr_prediction range facet across RWCs separated by age
summary_df <- df_ %>%
  group_by(act_fr, RWC, age) %>%
  summarise(
    min_pred_fr = round(min(pred_fr), 2),
    max_pred_fr = round(max(pred_fr), 2),
    .groups = 'drop'
  )

fr_ratio_plot <- ggplot(df_, aes(x = factor(act_fr), y = pred_fr, fill = factor(age))) +
  geom_boxplot_pattern(alpha = 0.6, outlier.size = 2, color = "black", pattern_angle = 45) +
  geom_text(data = summary_df, aes(x = factor(act_fr), label = format(min_pred_fr, digits = 3), y = min_pred_fr), 
            vjust = 1.1, color = "blue", position = position_dodge(width = 0.75)) +
  geom_text(data = summary_df, aes(x = factor(act_fr), label = format(max_pred_fr, digits = 3), y = max_pred_fr), 
            vjust = -0.3, color = "red", position = position_dodge(width = 0.75)) +
  labs(x = "Actual Fraction", y = expression("Predicted " * f[r]), 
       title = paste0(df_$index_name[1]), color = "RWC", fill = "Age") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2),
        legend.text = element_text(size = base_size),
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5),
        axis.title = element_text(size = base_size * 1.2),
        axis.text = element_text(size = base_size, color = "black"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"),
        strip.text = element_text(size = base_size * 1.1)) +
  facet_wrap(~RWC, scales = "free", labeller = custom_labeller)

print(fr_ratio_plot)

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
fr_idx_df <- unfiltered_df
fr_idx_df <- fr_idx_df %>% dplyr::filter(RWC == 0)
base_size = 14
# sl <- unique(fr_idx_df$soil)[1]
for (sl in unique(fr_idx_df$soil)){
  for (idx_ in unique(fr_idx_df$index_name)){
    filtered <- fr_idx_df %>% dplyr::filter(soil == sl)
    filtered <- filtered %>% dplyr::filter(index_name == idx_)

    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(RWC), shape = as.factor(crop))) +
      geom_point(size = 4) + # Adds points to the plot
      geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_discrete(name = "RWC") +
      scale_shape_discrete(name = "Crop") + # Adds a legend for RWC
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(legend.position = "right",
            legend.title = element_text(size = base_size * 2), # Legend title larger than base size
            legend.text = element_text(size = base_size *1.5), # Legend text at base size
            # plot.title = element_text(size = base_size * 2, hjust = 0.5), # Title larger than base size
            axis.title = element_text(size = base_size * 2), # Axis titles larger than base size
            axis.text = element_text(size = base_size * 1.5, color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm")) +
      scale_y_continuous(limits = c(0, 1)) 

    # Print the plot
    print(p)
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/', sl, "_", filtered$index_name[1], '.png'), p, width = 10, height = 7, dpi = 300)
  }
}



#######################################
#######################################
#######################################
#######################################

######### Across crops and RWCs
df_ <- NDTI_df
# Creating the scatter plot
fr_ratio_plot <- ggplot(df_, aes(x = pred_fr, y = act_fr, shape = factor(crop))) +
  geom_point(alpha = 0.6, size = 3) +  # Plot points with some transparency
  geom_smooth(aes(group = RWC), method = "lm") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a 1:1 line in red
  labs(x = "Predicted Fraction", y = "Actual Fraction",
       title = paste0(df_$index_name[1], ""), shape = "Crop") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette for RWC
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 6, 8)) +  # Define shapes for each crop type
  scale_x_continuous(limits = c(0, 1.4), breaks = seq(0, 1, 0.2)) +  # Set x-axis from 0 to 1
  scale_y_continuous(limits = c(0, 1.4), breaks = seq(0, 1, 0.2)) +  # Set y-axis from 0 to 1
  theme(
    legend.position = "right",
    legend.title = element_text(size = base_size * 2),  # Legend title larger than base size
    legend.text = element_text(size = base_size * 2),  # Legend text at base size
    plot.title = element_text(size = base_size * 2, hjust = 0.5),  # Title larger than base size
    axis.title = element_text(size = base_size * 2),  # Axis titles larger than base size
    axis.text = element_text(size = base_size, color = "black"),  # Axis text at base size
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis ticks 45 degrees
    axis.text.y = element_text(angle = 45, hjust = 1),  # Rotate y-axis ticks 45 degrees
    panel.background = element_rect(fill = "white", colour = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white", colour = "white"),  # Set plot background to white
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    legend.key.size = unit(0.4, "cm"),
    strip.text = element_text(size = base_size * 2)
  ) +
  facet_wrap(~RWC, scales = "free", labeller = custom_labeller)  # Facet by RWC

print(fr_ratio_plot)
ggsave(paste0(path_to_plots, 'fr_pred_act_fits_Athena/', df_$index_name[1], '.png'), fr_ratio_plot, width = 13, height = 7, dpi = 300)



# Plot to check soil type influence 
dff <- unfiltered_df %>%
  dplyr::filter(RWC == 0.8)

base_size <- 14
# Creating the scatter plot across soils
plot <- ggplot(dff, aes(x = pred_fr, y = act_fr, color = factor(soil))) +
  geom_point(alpha = 0.6, size = 2) +  # Plot points with some transparency
  # geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a 1:1 line in red
  labs(x = "Predicted Fraction", y = "Actual Fraction",
       title = paste0(" Comparison of Predicted vs. Actual Fraction"),
       color = "soil") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Setting a color palette for RWC
  # scale_shape_manual(values = c(0, 1, 2, 3, 4, 6, 8)) +  # Define shapes for each crop type
  scale_x_continuous(limits = c(0, 1.4), breaks = seq(0, 1.4, 0.2)) +  # Set x-axis from 0 to 1
  scale_y_continuous(limits = c(0, 1.4), breaks = seq(0, 1.4, 0.2)) +  # Set y-axis from 0 to 1
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
  facet_grid(index_name ~ crop, scales = "free")

print(plot)
ggsave(paste0(path_to_plots, 'fr_pred_act_fits_Athena/', df_$index_name[1], '.png'), fr_ratio_plot, width = 13, height = 7, dpi = 300)



