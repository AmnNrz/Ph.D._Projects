library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/')

# ## Plot Reflect vs Wvl across RWCs
# Residue
residue = read.csv(paste0(path_to_data, "Residue.csv"))
residue <- residue %>%
  mutate(RWC = round(RWC, 2))

# Filter data for "Wheat Norwest Duet"
for (type in unique(residue$Type)){
  filtered_data <- residue %>%
    filter(Type == type)
  
  base_size <- 14
  
  # Plot
  plot <- ggplot(filtered_data, aes(x = Wvl, y = Reflect, color = as.factor(RWC))) +
    geom_line() + # Use geom_point() if you want points instead of lines
    labs(title = type,
         x = "Wavelength, nm",
         y = "Reflectance factor",
         color = "RWC") +
    theme_minimal() +
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
          legend.key.size = unit(0.5, "cm"))
  
  ggsave(paste0(path_to_plots, 'ref_wvl_rwc/residue/', type, '.png'), plot, width = 10, height = 7, dpi = 300)
}

# Soil
soil = read.csv(paste0(path_to_data, "Soil.csv"))
soil <- soil %>%
  mutate(RWC = round(RWC, 2))

# type = "Wheat Norwest Duet"
# Filter data for "Wheat Norwest Duet"
for (type in unique(soil$Type)){
  filtered_data <- soil %>%
    filter(Type == type)
  
  base_size <- 14
  
  # Plot
  plot <- ggplot(filtered_data, aes(x = Wvl, y = Reflect, color = as.factor(RWC))) +
    geom_line() + # Use geom_point() if you want points instead of lines
    labs(title = type,
         x = "Wavelength, nm",
         y = "Reflectance factor",
         color = "RWC") +
    theme_minimal() +
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
          legend.key.size = unit(0.5, "cm"))
  
  ggsave(paste0(path_to_plots, 'ref_wvl_rwc/soil/', type, '.png'), plot, width = 10, height = 7, dpi = 300)
}

#######################################
              #Dry signatures
#######################################

# Residue
driest_residue <- residue %>%
  group_by(Type) %>%
  filter(RWC == min(RWC)) %>%
  ungroup()

base_size <- 14

# Plot
plot <- ggplot(driest_residue, aes(x = Wvl, y = Reflect, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = "Dry signature",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "Crop") +
  theme_minimal() +
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
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'dry_sig/', 'residues.png'), plot, width = 10, height = 7, dpi = 300)


# Soil
driest_soil <- soil %>%
  group_by(Type) %>%
  filter(RWC == min(RWC)) %>%
  ungroup()

base_size <- 14

# Plot
plot <- ggplot(driest_soil, aes(x = Wvl, y = Reflect, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = "Dry signature",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "Soil") +
  theme_minimal() +
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
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'dry_sig/', 'soil.png'), plot, width = 10, height = 7, dpi = 300)

# Combined
merged_df <- rbind(driest_residue, driest_soil)
base_size <- 14

# Plot
plot <- ggplot(merged_df, aes(x = Wvl, y = Reflect, color = as.factor(Sample))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = "Dry signatures",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = " ") +
  theme_minimal() +
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
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'dry_sig/', 'merged.png'), plot, width = 10, height = 7, dpi = 300)


###################################
# 0.5 and dry RWC difference
###################################

######## Residue ########
# Assuming your data frame is named 'df'
df <- residue


# Calculate minimum RWC and RWC closest to 0.5 for each crop type
min_and_closest_rwc <- df %>%
  group_by(Type) %>%
  summarise(
    Min_RWC = min(RWC),
    Closest_RWC = RWC[which.min(abs(RWC - 0.5))]
  ) %>%
  ungroup()

# Filter original data to include only rows matching the Min_RWC or Closest_RWC for each Type
filtered_df <- df %>%
  inner_join(min_and_closest_rwc, by = "Type") %>%
  filter(RWC == Min_RWC | RWC == Closest_RWC)

# Separate the data into two parts for easier manipulation
driest_df <- filtered_df %>% filter(RWC == Min_RWC)
closest_df <- filtered_df %>% filter(RWC == Closest_RWC)

# Assuming same wavelengths are used in all measurements and each type has a unique driest and closest RWC
combined_df <- driest_df %>%
  select(Type, Wvl, Reflect) %>%
  rename(Driest_Reflect = Reflect) %>%
  inner_join(closest_df %>% select(Type, Wvl, Reflect) %>% rename(Closest_Reflect = Reflect), by = c("Type", "Wvl"))

# Calculate the difference
combined_df <- combined_df %>%
  mutate(ref_dif = Driest_Reflect - Closest_Reflect)

base_size <- 14

# Plot
plot <- ggplot(combined_df, aes(x = Wvl, y = ref_dif, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = 'reflectance difference (Driest & ~0.5 RWC)',
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "") +
  theme_minimal() +
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
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'Reflect_difference/', 'residue_0.5.png'), plot, width = 10, height = 7, dpi = 300)

######## Soil ########
# Assuming your data frame is named 'df'
df <- soil


# Calculate minimum RWC and RWC closest to 0.5 for each crop type
min_and_closest_rwc <- df %>%
  group_by(Type) %>%
  summarise(
    Min_RWC = min(RWC),
    Closest_RWC = RWC[which.min(abs(RWC - 0.5))]
  ) %>%
  ungroup()

# Filter original data to include only rows matching the Min_RWC or Closest_RWC for each Type
filtered_df <- df %>%
  inner_join(min_and_closest_rwc, by = "Type") %>%
  filter(RWC == Min_RWC | RWC == Closest_RWC)

# Separate the data into two parts for easier manipulation
driest_df <- filtered_df %>% filter(RWC == Min_RWC)
closest_df <- filtered_df %>% filter(RWC == Closest_RWC)

# Assuming same wavelengths are used in all measurements and each type has a unique driest and closest RWC
combined_df <- driest_df %>%
  select(Type, Wvl, Reflect) %>%
  rename(Driest_Reflect = Reflect) %>%
  inner_join(closest_df %>% select(Type, Wvl, Reflect) %>% rename(Closest_Reflect = Reflect), by = c("Type", "Wvl"))

# Calculate the difference
combined_df <- combined_df %>%
  mutate(ref_dif = Driest_Reflect - Closest_Reflect)

base_size <- 14

# Plot
plot <- ggplot(combined_df, aes(x = Wvl, y = ref_dif, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = 'reflectance difference (Driest & ~0.5 RWC)',
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "") +
  theme_minimal() +
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
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'Reflect_difference/', 'soil_0.5.png'), plot, width = 10, height = 7, dpi = 300)


###################################
# All RWCs and driest difference
###################################

# Assuming your data frame is named 'df'
df <- soil

# Identify the driest (minimum RWC) for each crop type
driest_df <- df %>%
  group_by(Type) %>%
  summarise(Min_RWC = min(RWC)) %>%
  ungroup() %>%
  inner_join(df, by = "Type") %>%
  filter(RWC == Min_RWC) %>%
  select(-Min_RWC)

# Calculate reflectance difference for each RWC level compared to the driest
reflectance_diff_df <- df %>%
  inner_join(driest_df, by = c("Type", "Wvl"), suffix = c("", "_driest")) %>%
  mutate(ref_dif = Reflect_driest - Reflect, 
         Difference_Label = paste("Diff to RWC", RWC)) %>%
  select(Type, Wvl, RWC, Reflect, Reflect_driest, ref_dif, Difference_Label)


for (type in unique(reflectance_diff_df$Type)){
  filtered_df <- reflectance_diff_df %>%
    dplyr::filter( Type == type)
  base_size <- 14
  
  # Plot
  plot <- ggplot(filtered_df, aes(x = Wvl, y = ref_dif, color = as.factor(Difference_Label))) +
    geom_line() + # Use geom_point() if you want points instead of lines
    labs(title = type,
         x = "Wavelength, nm",
         y = "Reflectance factor",
         color = "") +
    theme_minimal() +
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
          legend.key.size = unit(0.5, "cm"))
  print(plot)
  ggsave(paste0(path_to_plots, 'Reflect_difference/all_RWCs/soil/', type, '.png'), plot, width = 10, height = 7, dpi = 300)
  
}



