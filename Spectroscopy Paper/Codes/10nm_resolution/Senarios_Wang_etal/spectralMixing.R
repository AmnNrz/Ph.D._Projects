library(tidyverse)
library(dplyr)
library(ggplot2)

# path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
#                        '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')


Residue <- read.csv(paste0(path_to_data, "Residue.csv"))
Residue <- Residue[, -c(1, ncol(Residue))]
Soil <- read.csv(paste0(path_to_data, "Soil.csv"))
Soil <- Soil[, -c(1, ncol(Soil))]

res_wide <- Residue %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 
Soil_wide <- Soil %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 


# Check common RWC ranges
res_ = res_wide[, 1:4]
soil_ = Soil_wide[, 1:4]

res_ <- res_ %>% rename(Type_Name = Crop)
soil_ <- soil_ %>% rename(Type_Name = Soil)

res_soil_ <- rbind(res_, soil_) 

write.csv(res_soil_, file = paste0(path_to_data, "res_soil_.csv"), row.names = FALSE)

ggplot(res_soil_, aes(x=RWC)) + 
  geom_histogram(breaks=seq(0, 1, by=0.1), fill="lightblue", color="black", alpha=0.7) +
  facet_wrap(~Type_Name, scales="free_y") + 
  labs(title="Distribution of RWC across different soils and crop residues",
       x="RWC", 
       y="Count") +
  scale_x_continuous(breaks=seq(0, 1, by=0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Cross join
combined_data <- merge(Residue, Soil, by = NULL)

# Filter rows with close RWC values
close_values <- combined_data %>% 
  filter(abs(RWC.x - RWC.y) <= 5) %>%
  select(soil, crop, RWC.x, RWC.y)

print(close_values)
