
# Load the reshape2 package
library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# Set the path to directory

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/',
#                        '10nm_resolution/index_org_trsfed_crp_sl/')
# 
# path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/',
#                         '10nm_resolution/FR_Index_newBands2/')


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/',
                       '10nm_resolution/index_org_trsfed_crp_sl/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/',
                        '10nm_resolution/FR_Index_newBands2/')

# Get a list of all .csv files in the directory
csv_files <- list.files(path = path_to_data, pattern = "\\.csv$", full.names = FALSE)

# Remove the ".csv" extension from the file names
csv_files <- sub("\\.csv$", "", csv_files)

# Extract characters after the second underscore
csv_files <- sapply(strsplit(csv_files, "_"), function(x) paste(tail(x, -2), collapse = "_"))

print(unique(csv_files))



##################################################################
##################################################################
##################################################################

########################### AFTER EPO ###########################

##################################################################
##################################################################
##################################################################
name <- unique(csv_files)[1]
  for (name in unique(csv_files)) {

  NDTI_transformed <- read.csv(paste0(path_to_data, 'NDTI_transformed_',name, '.csv'))
  NDTI_original <- read.csv(paste0(path_to_data, 'NDTI_Original_', name, '.csv'))
  
  CAI_transformed <- read.csv(paste0(path_to_data, 'CAI_transformed_', name, '.csv'))
  CAI_original <- read.csv(paste0(path_to_data, 'CAI_Original_', name, '.csv'))
  
  SINDRI_transformed <- read.csv(paste0(path_to_data, 'SINDRI_transformed_', name, '.csv'))
  SINDRI_original <- read.csv(paste0(path_to_data, 'SINDRI_Original_', name, '.csv'))
  
  ##Fig5
  ggplot(NDTI_transformed, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
    geom_line(aes(color = factor(RWC.y))) +
    geom_point(aes(shape = factor(RWC.y)))+
    labs(y = "Fraction Residue Cover", x = "NDTI") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
          legend.title=element_blank(),
          legend.margin=margin(c(1,5,5,5)))
  
  ##################################################################
  ##################################################################
  ##################################################################
  
  
  ##Fig5
  ggplot(CAI_transformed, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
    geom_line(aes(color = factor(RWC.y))) +
    geom_point(aes(shape = factor(RWC.y)))+
    labs(y = "Fraction Residue Cover", x = "CAI") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
          legend.title=element_blank(),
          legend.margin=margin(c(1,5,5,5)))
  
  ##################################################################
  ##################################################################
  ##################################################################
  
  
  ##Fig5
  ggplot(SINDRI_transformed, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
    geom_line(aes(color = factor(RWC.y))) +
    geom_point(aes(shape = factor(RWC.y)))+
    labs(y = "Fraction Residue Cover", x = "SINDRI") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
          legend.title=element_blank(),
          legend.margin=margin(c(1,5,5,5)))
  
  
  
  ##################################################################
  ##################################################################
  ##################################################################
  
  ########################### BEFORE EPO ###########################
  
  ##################################################################
  ##################################################################
  ##################################################################
  
  ##Fig5
  ggplot(NDTI_original, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
    geom_line(aes(color = factor(RWC.y))) +
    geom_point(aes(shape = factor(RWC.y)))+
    labs(y = "Fraction Residue Cover", x = "NDTI") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
          legend.title=element_blank(),
          legend.margin=margin(c(1,5,5,5)))
  
  ##################################################################
  ##################################################################
  ##################################################################
  
  
  ##Fig5
  ggplot(CAI_original, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
    geom_line(aes(color = factor(RWC.y))) +
    geom_point(aes(shape = factor(RWC.y)))+
    labs(y = "Fraction Residue Cover", x = "CAI") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
          legend.title=element_blank(),
          legend.margin=margin(c(1,5,5,5)))
  
  ##################################################################
  ##################################################################
  ##################################################################
  
  
  ##Fig5
  ggplot(SINDRI_original, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
    geom_line(aes(color = factor(RWC.y))) +
    geom_point(aes(shape = factor(RWC.y)))+
    labs(y = "Fraction Residue Cover", x = "SINDRI") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
          legend.title=element_blank(),
          legend.margin=margin(c(1,5,5,5)))
  
  # Combine the datasets
  
  # For NDTI
  NDTI_transformed$index <- 'After EPO'
  NDTI_transformed$type <- 'NDTI'
  NDTI_original$index <- 'Before EPO'
  NDTI_original$type <- 'NDTI'
  
  # For CAI
  CAI_transformed$index <- 'After EPO'
  CAI_transformed$type <- 'CAI'
  CAI_original$index <- 'Before EPO'
  CAI_original$type <- 'CAI'
  
  # For SINDRI
  SINDRI_transformed$index <- 'After EPO'
  SINDRI_transformed$type <- 'SINDRI'
  SINDRI_original$index <- 'Before EPO'
  SINDRI_original$type <- 'SINDRI'
  
  SINDRI_transformed$CAI <- SINDRI_transformed$CAI/100
  
  # Combine all datasets into one
  combined_data <- rbind(NDTI_original, NDTI_transformed, CAI_original, CAI_transformed, SINDRI_original, SINDRI_transformed)
  
  
  # Change the order of levels of 'index'
  combined_data$index <- factor(combined_data$index, levels = c('Before EPO', 'After EPO'))
  
  combined_data <- subset(combined_data, index == 'Before EPO')
  combined_data$RWC.y <- round(combined_data$RWC.y, 2)
  # Get all unique RWC levels from both dataframes
  all_rwc_levels <- unique(c(combined_data$RWC.y))
  
  # Create a color palette with enough colors for all RWC levels
  custom_colors <- viridis(length(all_rwc_levels))
  custom_colors <- rev(custom_colors)
  
  # Plotting
    plot <- ggplot(combined_data, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
    geom_line() +
    geom_point(aes(shape = factor(RWC.y), color = factor(RWC.y))) +
    scale_color_manual(values = custom_colors, name = "RWC Levels") +  # Set legend title here
    labs(title = paste0("'", strsplit(name, "_")[[1]][1], "'"," ", "on", " ", "'", strsplit(name, "_")[[1]][2], "'"),
         y = "Fraction Residue Cover") +  # Removed the 'shape' title here
    xlab("") +  # Set X axis label to an empty string to remove it
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  # This line centers the title
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),           # Remove major grid lines
      panel.grid.minor = element_blank(),           # Remove minor grid lines
      strip.background = element_rect(fill = "white"), # Remove facet strip background
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "right",
      legend.margin = margin(c(1, 5, 5, 5)),
      legend.key.size = unit(0.4, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text to 45 degrees
    ) +
      facet_wrap(index ~ type,scales = "free")+
    guides(
      shape = FALSE,  # Remove the shape legend
      color = guide_legend(
        override.aes = list(shape = 1:length(custom_colors))  # Set the shapes in the legend
      )
    )
  print(plot)
  
    # Save the figure as a PDF with A5 size (width = 14.8 cm, height = 21 cm)
    # ggsave(paste0(path_to_plots, name, ".png"), width = 22, height = 14.8, units = "cm")
}



########################################################################
########################################################################
########################################################################
library(dplyr)
library(magrittr) 
setwd(path_to_data)


# List all files that start with 'NDTI_original_' and end with '_Shano.csv'
# List all files that start with 'CAI_Original_' and end with 'Shano'
file_names <- list.files(pattern = "^CAI_Original_.*Shano\\.csv$")

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each file name, read the file, and add it to the list
for (file in file_names) {
  data_frame <- read.csv(file, stringsAsFactors = FALSE)
  crop_name <- sub("^CAI_Original_(.*)_Shano\\.csv$", "\\1", file)
  
  # Add the 'Type' column with the crop name
  data_frame$Type <- crop_name
  data_list[[length(data_list) + 1]] <- data_frame
}

# Combine all data frames into one
combined_df <- bind_rows(data_list)





# Load necessary libraries
library(dplyr)
library(broom)

# Group by 'RWC.y' and perform linear model fitting
model_results <- combined_df %>%
  group_by(RWC.y) %>%
  do(tidy(lm(CAI ~ Fraction_Residue_Cover, data = .)))

# Select the results of interest, which are the coefficients (intercept and slope)
slopes <- model_results %>%
  filter(term == "Fraction_residue_cover") %>%
  select(RWC.y, slope = estimate)

intercepts <- model_results %>%
  filter(term == "(Intercept)") %>%
  select(RWC.y, intercept = estimate)

# Join these results back to the original dataframe
combined_df <- combined_df %>%
  left_join(slopes, by = "RWC.y") %>%
  left_join(intercepts, by = "RWC.y")

# Rename columns for clarity
combined_df <- combined_df %>%
  rename(SampleStatus = slope, Intercept = intercept)

# Print the updated combined data frame to view its contents along with the new columns
print(combined_df)





# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming 'combined_df' is already created and contains the columns needed
# Binning the RWC.y into categories (this step may need adjustments based on your actual RWC.y data distribution)
combined_df$RWC.y.binned <- cut(combined_df$RWC.y, breaks = 10)  # Adjust the number of breaks as needed

# Preparing data for plotting
heatmap_data <- combined_df %>%
  select(Type, RWC.y.binned, Intercept) %>%
  group_by(Type, RWC.y.binned) %>%
  summarize(Average_Slope = mean(Intercept, na.rm = TRUE), .groups = 'drop')


# Plotting the heatmap
hea35tmap_plot <- ggplot(heatmap_data, aes(x = RWC.y.binned, y = Type, fill = Average_Slope)) +
  geom_tile() +  # Use geom_tile for heatmap squares
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(min(heatmap_data$Average_Slope),
                                 max(heatmap_data$Average_Slope)), 
                       space = "Lab", name="Intercept") +
  labs(title = "Heatmap of Slopes by Crop Type and Moisture Level", x = "Binned Moisture Level", y = "Crop Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white")  # Ensures background is white
  )

# Display the plot

print(heatmap_plot)

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/')

ggsave(paste0(path_to_plots, 'Slop_intercept_heatmap/', 'Intercept_Shano.png'), heatmap_plot, width = 10, height = 7, dpi = 300)


