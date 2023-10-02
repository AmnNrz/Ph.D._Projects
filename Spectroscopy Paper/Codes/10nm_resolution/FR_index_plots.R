
# Load the reshape2 package
library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# Set the path to directory
path_to_directory <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
       '/Ph.D/Projects/Spectroscopy_Paper/Data/',
       'Updated_data_2_10nm_res/index_org_trsfed_crp_sl_all/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Spectroscopy_Paper/Plots/')

# Get a list of all .csv files in the directory
csv_files <- list.files(path = path_to_directory, pattern = "\\.csv$", full.names = FALSE)

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

for (name in unique(csv_files)) {

  NDTI_transformed <- read.csv(
    paste0(path_to_directory, 'NDTI_transformed_',name, '.csv')
    )
  NDTI_original <- read.csv(
    paste0(path_to_directory, 'NDTI_Original_', name, '.csv')
    )
  
  CAI_transformed <- read.csv(
    paste0(path_to_directory, 'CAI_transformed_', name, '.csv')
    )
  CAI_original <- read.csv(
    paste0(path_to_directory, 'CAI_Original_', name, '.csv')
    )
  
  SINDRI_transformed <- read.csv(
    paste0(path_to_directory, 'SINDRI_transformed_', name, '.csv')
    )
  SINDRI_original <- read.csv(
    paste0(path_to_directory, 'SINDRI_Original_', name, '.csv')
    )
  
  ##################################################################
  ##################################################################
  ##################################################################
  
  ###################.          BEFORE EPO.      ##################

  
  ##################################################################
  ##################################################################
  ##################################################################
  
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
  combined_data <- rbind(NDTI_original, NDTI_transformed,
                         CAI_original, CAI_transformed,
                         SINDRI_original, SINDRI_transformed)
  # Change the order of levels of 'index'
  combined_data$index <- factor(combined_data$index, levels = c('Before EPO', 'After EPO'))
  
  # Get all unique Scan levels from both dataframes
  all_Scan_levels <- unique(c(combined_data$Scan))
  
  # Create a color palette with enough colors for all Scan levels
  custom_colors <- viridis(length(all_Scan_levels))
  custom_colors <- rev(custom_colors)
  
  # Plotting
    ggplot(combined_data, aes(CAI, Fraction_Residue_Cover, group = factor(Scan))) +
    geom_line(aes(color = factor(Scan))) +
    geom_point(aes(shape = factor(Scan), color = factor(Scan))) +
      scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
    scale_color_manual(values = custom_colors, name = "Scan Levels") +  # Set legend title here
    labs(title = "Fresh Residues - All soils", 
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
      legend.key.size = unit(0.4, "cm")
    ) +
    facet_grid(index ~ type, scales = "free") +
    guides(
      shape = FALSE,  # Remove the shape legend
      color = guide_legend(
        override.aes = list(shape = 1:length(custom_colors))  # Set the shapes in the legend
      )
    )
  
    # Save the figure as a PDF with A5 size (width = 14.8 cm, height = 21 cm)
    ggsave(paste0(path_to_plots,"freshRes_allSoils/", "Fr ~ Index_", name, ".png"), width = 21, height = 14.8, units = "cm")
}




