library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')


Residue <- read.csv(paste0(path_to_data, "Residue.csv"))
Residue <- Residue[, -c(1, ncol(Residue))] %>% 
  mutate(Sample = ifelse(is.character('Crop Residue'), 'Residue', Sample))

Soil <- read.csv(paste0(path_to_data, "Soil.csv"))

Soil <- Soil[, -c(1, ncol(Soil))]

res_wide <- Residue %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 
Soil_wide <- Soil %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 

res_wide <- res_wide %>% rename(Type = Crop)
Soil_wide <- Soil_wide %>% rename(Type = Soil)

###############################################################
###############################################################
# Check common RWC ranges using hisotogram of RWC
res_ = res_wide[, 1:4]
soil_ = Soil_wide[, 1:4]

res_ <- res_ %>% rename(Type = Crop)
soil_ <- soil_ %>% rename(Type = Soil)

res_soil_ <- rbind(res_, soil_) 

write.csv(res_soil_, file = paste0(path_to_data, "res_soil_.csv"),
          row.names = FALSE)

ggplot(res_soil_, aes(x=RWC)) + 
  geom_histogram(breaks=seq(0, 1, by=0.1), fill="lightblue", color="black",
                 alpha=0.7) +
  facet_wrap(~Type_Name, scales="free_y") + 
  labs(title="Distribution of RWC across different soils and crop residues",
       x="RWC", 
       y="Count") +
  scale_x_continuous(breaks=seq(0, 1, by=0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################
###############################################################
# crops = unique(CAI[CAI$Sample == "Residue", ]$Type)
# soils = unique(CAI[CAI$Sample == "Soil", ]$Type)

common_cols <- intersect(names(res_wide), names(Soil_wide))
res_wide <- res_wide[, common_cols]

df <- rbind(res_wide, Soil_wide)

df <- df %>%
  mutate(`RWC range` = case_when(
    RWC >= 0.0 & RWC <= 0.25 ~ "0-0.25",
    RWC >= 0.48 & RWC <= 0.65 ~ "0.48-0.65",
    RWC >= 0.80 & RWC <= 1 ~ "0.85-1",
    TRUE ~ "Other"  # Optional: Handle values outside of specified ranges
  )) %>%
  select(1:3, "RWC range", everything())

df <- df %>%
  filter(`RWC range` != "Other")

df <- df %>%
  rowwise() %>%
  mutate(fr = list(seq(0, 1, by = 0.1))) %>%
  unnest(cols = c(fr)) %>% 
  select(1:5, "fr", everything())









# mixed_df <- data.frame()
# for (crp in crops) {

Peas <- dplyr::filter(df, Type == "Peas")

Athena <- dplyr::filter(df, Type == "Athena")
    
  
# }
  







