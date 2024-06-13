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

Xsr_combined <- read.csv(paste0(path_to_data, "Xsr_combined_allCrop_allSoils.csv"))

mix <- unique(Xsr_combined$Mix)[1]

for (mix in unique(Xsr_combined$Mix)){
  Xsr_mix <- dplyr::filter(Xsr_combined, Mix == mix)
  
  Xsr_filtered <- dplyr::filter(Xsr_mix, Fraction == 0 | Fraction == 1 | Fraction== 0.5)
  
  Xsr_filtered$source <- factor(Xsr_filtered$source, levels = c("Original", "EPO"))
  Xsr_filtered$RWC_factor <- as.factor(round(Xsr_filtered$RWC, 2))
  plot <- ggplot(Xsr_filtered, aes(x = Wvl, y = Reflect, group = RWC_factor)) + 
    geom_line(aes(color = RWC_factor), size = 0.18) + 
    facet_wrap(~ source, ncol=2, scales = "free_y") +  
    # scale_color_viridis_d(direction = -1) + 
    scale_color_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
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
  ggsave(filename = paste0(path_to_plots, "BeforeAfterEPO_Reflect_wang/individual_mixes_all_rwc/", mix, ".png"), plot = plot, dpi = 200, width = 10, height = 3.5, units = "in")
  
  # Convert RWC into a factor (discrete variable)
  Xsr_mix$RWC_factor <- as.factor(round(Xsr_mix$RWC, 2))
  Xsr_mix$source <- factor(Xsr_mix$source, levels = c("Original", "EPO"))
  Xsr_mix$Fraction_factor <- as.factor(Xsr_mix$Fraction)
  
  
  # Now update ggplot2 code to use this new factor
  plot2 <- ggplot(Xsr_mix, aes(x = Wvl, y = Reflect, group = interaction(Fraction, RWC_factor))) + 
    geom_line(aes(color = Fraction, linetype = RWC_factor), linewidth = 0.3) + 
    facet_wrap(~ source, ncol=2, scales = "free_y") +  
    scale_color_viridis_c(direction = -1) + 
    # scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    labs(
      title = "Reflectance ~ Wavelength",
      x = "Wavelength", y = "Reflectance",
      color = "Fraction",
      linetype = "RWC Levels" # Updated label for clarity
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
  
  print(plot2)

  Xsr_mix_ <- dplyr::filter(Xsr_mix, Fraction==0 | Fraction==1| Fraction==0.5)
  plot3 <- ggplot(Xsr_mix_, aes(x = Wvl, y = Reflect, group = interaction(RWC_factor, Fraction_factor))) + 
    geom_line(aes(color = RWC_factor, linetype = Fraction_factor), linewidth = 0.3) + 
    facet_wrap(~ source, ncol=2, scales = "free_y") +  
    # scale_color_viridis_d(direction = -1) + 
    scale_color_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "twodash", "twodash", "twodash", "twodash", "twodash")) +
    scale_x_continuous(breaks = seq(min(Xsr_filtered$Wvl, na.rm = TRUE), max(Xsr_filtered$Wvl, na.rm = TRUE), by = 100), # Major breaks every 100 units
                       minor_breaks = seq(min(Xsr_filtered$Wvl, na.rm = TRUE), max(Xsr_filtered$Wvl, na.rm = TRUE), by = 10)) + # Minor breaks every 50 units
    labs(
      title = "Reflectance ~ Wavelength",
      x = "Wavelength", y = "Reflectance",
      color = "RWC Levels",
      linetype = "Fraction"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(color = "grey", size = 0.2), # Customizing minor grid lines
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_line(color = "black"),
          strip.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")
    )
  
  print(plot3)
  
  
  
  
  ggsave(filename = paste0(path_to_plots, "BeforeAfterEPO_Reflect_wang/individual_mixes_all_fr_rwc_fr_1_0/", mix, ".png"), plot = plot3, dpi = 200, width = 15, height = 7, units = "in")
}



# Xsr_filtered <- dplyr::filter(Xsr_combined, Mix == "Peas_Athena")
# Xsr_filtered <- Xsr_combined
fractions <- sort(unique(Xsr_filtered$Fraction))
RWCs <- sort(unique(Xsr_filtered$RWC))
# fr <- fractions[2]
# rwc1 <- RWCs[2]
# rwc2 <- RWCs[3]
# Xsr_filtered <- dplyr::filter(Xsr_combined, Fraction == fr)
# Xsr_filtered <- dplyr::filter(Xsr_combined, RWC == rwc1 | RWC == rwc2)
# Xsr_filtered <- dplyr::filter(Xsr_filtered, RWC == rwc1)

# Xsr_filtered$source <- factor(Xsr_filtered$source, levels = c("Original", "EPO"))
Xsr_combined$source <- factor(Xsr_combined$source, levels = c("Original", "EPO"))

plot <- ggplot(Xsr_filtered, aes(x = Wvl, y = Reflect, group = Fraction)) + 
geom_line(aes(color = Fraction), size = 0.18) + 
facet_wrap(~ source, ncol=2) +  
scale_color_viridis_c(direction = -1) + 
labs(
  title = "Reflectance ~ Wavelength",
  x = "Wavelength", y = "Reflectance",
  color = "Fraction"
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


library(ggplot2)
library(viridis)

# Convert RWC into a factor (discrete variable)
Xsr_filtered$RWC_factor <- as.factor(Xsr_filtered$RWC)
Xsr_combined$RWC_factor <- as.factor(Xsr_combined$RWC)



plot <- ggplot(Xsr_combined, aes(x = Wvl, y = Reflect, group = interaction(Fraction, RWC_factor))) + 
  geom_line(aes(color = Fraction, linetype = RWC_factor), size = 0.18) + 
  facet_wrap(~ source, ncol=2, scales = "free_y") +  
  scale_color_viridis_c(direction = -1) + 
  # scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(
    title = "Reflectance ~ Wavelength",
    x = "Wavelength", y = "Reflectance",
    color = "Fraction",
    linetype = "RWC Levels" # Updated label for clarity
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
