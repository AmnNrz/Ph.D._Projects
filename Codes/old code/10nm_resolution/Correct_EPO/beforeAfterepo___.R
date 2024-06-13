library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/Wangetal_last0/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# 
# path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/Wangetal2')

# Xsr_combined <- read.csv(paste0(path_to_data, "Xsr_combined_allCrop_allSoils.csv"))




Soil_transformed <- read.csv(paste0(path_to_data, "Soil_Transformed.csv"),header = TRUE, row.names = NULL)
Soil <- read.csv(paste0(path_to_data,
                        "Soil.csv"),
                 header = TRUE, row.names = NULL)
Soil <- Soil[-c(1, 4, 8)]
Soil <- dplyr::filter(Soil, Wvl >=500)

Soil <- Soil %>%
  rename(Type = Soil)

# Min-Max Normalization
min_max_normalize <- function(matrix) {
  min_val <- min(matrix)
  max_val <- max(matrix)
  normalized_matrix <- (matrix - min_val) / (max_val - min_val)
  return(normalized_matrix)
}



Soil_transformed <- Soil_transformed[names(Soil)]

Soil$Source <- 'Original'
Soil_transformed$Source <- "EPO"

Soil_combined <- rbind(Soil, Soil_transformed)
type <- unique(Soil_combined$Type)[1]
for (type in unique(Soil_combined$Type)){
  df <- dplyr::filter(Soil_combined, Type == type)
  
  df$Source <- factor(df$Source, levels = c("Original", "EPO"))
  df$RWC_factor <- as.factor(round(df$RWC, 2))
  plot <- ggplot(df, aes(x = Wvl, y = Reflect, group = RWC_factor)) + 
    geom_line(aes(color = RWC_factor), linewidth = 0.18) + 
    facet_wrap(~ Source, ncol=2, scales = "free_y") +  
    scale_color_viridis_d(direction = -1) + 
    labs(
      title = "Reflectance ~ Wavelength",
      x = "Wavelength", y = "Reflectance",
      color = "RWC"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),        
          panel.grid.minor = element_blank(),       
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_line(color = "black"),
          strip.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")
    )
  
  print(plot)
  ggsave(filename = paste0(path_to_plots, "BeforeAfterEPO_Reflect_wang/individua_notNormalised_soil/",type, ".png"), plot = plot, dpi = 200, width = 10, height = 3.5, units = "in")
  
}

  