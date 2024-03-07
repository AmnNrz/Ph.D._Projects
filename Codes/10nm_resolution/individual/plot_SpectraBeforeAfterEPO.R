library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


# path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# 
# path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                         'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/Wangetal2/')

path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/Wangetal2')

Xsr_combined <- read.csv(paste0(path_to_data, "Xsr_combined.csv"))


for (mix in unique(Xsr_combined$Mix)){
  Xsr_mix <- dplyr::filter(Xsr_combined, Mix == mix)
  
  fr <- unique(Xsr_mix$Fraction)[2]
  Xsr_filtered <- dplyr::filter(Xsr_mix, Fraction == fr)
  
  Xsr_filtered$source <- factor(Xsr_filtered$source, levels = c("Original", "EPO"))
  
  plot <- ggplot(Xsr_filtered, aes(x = Wvl, y = Reflect, group = RWC)) + 
    geom_line(aes(color = RWC), size = 0.18) + 
    facet_wrap(~ source, ncol=2) +  
    scale_color_viridis_c(direction = -1) + 
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
  
  
  # Assuming your plot is named 'plot'
  ggsave(filename = paste0(path_to_plots, "BeforeAfterEPO_Reflect_wang/", mix, ".png"), plot = plot, dpi = 200, width = 10, height = 3.5, units = "in")
}
