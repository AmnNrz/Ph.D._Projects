library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


# path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
#                        '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')
Xsr_combined <- read.csv(paste0(path_to_data, "Xsr_combined.csv"))

mix <- "Peas_Athena"
Xsr_mix <- dplyr::filter(Xsr_combined, Mix == mix)

fr <- unique(Xsr_mix$Fraction)[1]
Xsr_mix <- dplyr::filter(Xsr_combined, Fraction == fr)


# Create the plot
plot <- ggplot(Xsr_mix, aes(x = Wvl, y = Reflect, group = RWC, color = RWC)) + 
  geom_line() + 
  facet_wrap(~ source, ncol=2) +  # Separate the plot based on 'source' column
  scale_color_viridis_c() +       # Use viridis color palette
  labs(title = "Reflect vs Wvl for different RWC", x = "Wvl", y = "Reflect") +
  theme_minimal()

print(plot)
