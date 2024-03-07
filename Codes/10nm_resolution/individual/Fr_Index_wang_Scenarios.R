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
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/Scenarios/')

Xsr_combined <- read.csv(paste0(path_to_data, "Xsr_combined_fresh_dark.csv"))
Xsr_combined_indices <- read.csv(paste0(path_to_data,
                                        "Xsr_Original_Transformed_indices_fresh_dark.csv"))


split_mix <- strsplit(Xsr_combined_indices$Mix, "_")
Xsr_combined_indices$Crop <- sapply(split_mix, "[[", 1)
Xsr_combined_indices$Soil <- sapply(split_mix, "[[", 2)

subset_df <- Xsr_combined_indices[
  grep(paste0("^", Xsr_combined_indices$Crop[1], "_"), Xsr_combined_indices$Mix), ] 

# Get all unique RWC levels from both dataframes
all_rwc_levels <- unique(c(subset_df$RWC))

# Create a color palette with enough colors for all RWC levels
custom_colors <- viridis::viridis(length(all_rwc_levels))
custom_colors <- rev(custom_colors)

# 1. Create a named vector for shapes
unique_rwc <- unique(subset_df$RWC)
shape_list <- c(16, 17, 18, 19, 20, 21, 22, 23) # adjust as needed
named_shapes <- setNames(shape_list[1:length(unique_rwc)], sort(unique_rwc))


subset_df_long <- subset_df %>% 
  pivot_longer(c("CAI", "NDTI", "SINDRI"), names_to = 'Index',
               values_to = 'Index_val')

subset_df_long$source <- factor(subset_df_long$source, levels = c("Original", "EPO"))


subset_df_long_ <- dplyr::filter(subset_df_long, Mix == subset_df_long$Mix[1])

# 2. Plot with combined legend
p <- ggplot(subset_df_long_, aes(x = Index_val, y =Fraction , color = factor(round(RWC, digits = 2)),
                               shape = factor(RWC))) +
    geom_smooth(method = "lm", se = FALSE, aes(group = factor(RWC))) +
    labs(
      title = paste0(subset_df_long$Mix[1]),
      x = "Index", y = "Residue Cover Fraction",
      color = "RWC"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),        
          panel.grid.minor = element_blank(),       
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_line(color = "black"),
          strip.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"))+
    # geom_point(size = 1) +
    scale_color_manual(values = custom_colors, name = "RWC Levels") +
    scale_shape_manual(values = named_shapes, name = "RWC Levels") + 
    guides(color = guide_legend(override.aes = list(size = 4))) +
    facet_grid(rows = vars(source), cols = vars(Index) , scales = "free")
    # facet_wrap(index ~ type,scales = "free")

# print(p)

p2 <- ggplot(subset_df_long_, aes(x = Index_val, y = Fraction, color = factor(round(RWC, digits = 2)),
                                 shape = factor(RWC))) +
  geom_smooth(method = "lm", se = FALSE, aes(group = factor(RWC))) +
  labs(
    title = paste0(subset_df_long$Mix[1]),
    x = "Index", y = "Residue Cover Fraction",
    color = "RWC"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),        
        panel.grid.minor = element_blank(),       
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(color = "black"),
        strip.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_color_manual(values = custom_colors, name = "RWC Levels") +
  scale_shape_manual(values = named_shapes, name = "RWC Levels") + 
  guides(color = guide_legend(override.aes = list(size = 4))) +
  facet_wrap(~source + Index, scales = "free", ncol = 3)
# Adjusted to use facet_wrap with an interaction of 'source' and 'Index'.
# Note: `ncol = 1` or another arrangement may be specified based on layout preference.

print(p2)

ggsave(filename = paste0(path_to_plots, subset_df_long$Mix[1], ".png"), plot = p2, dpi = 200, width = 10, height = 3.5, units = "in")


## ******               ******##
      # Fit the best lines
## ******               ******##

indices <- c("NDTI", "CAI", "SINDRI")


src <- "Original"
idx <- "NDTI"
fitted_df <- data.frame()
for (src in unique(subset_df_long$source)){
  subset_df_ <- dplyr::filter(subset_df_long, source == src)
  source_df <- data.frame()
  for (idx in indices){
    subset_df_idx <- dplyr::filter(subset_df_, Index==idx)
    model <- lm(subset_df_idx$Fraction ~ subset_df_idx$Index_val,
                 data = subset_df_idx)
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    # Calculate R^2
    summary_model <- summary(model)
    r_squared <- summary_model$r.squared
    
    # Calculate RMSE
    residuals <- model$residuals
    rmse <- sqrt(mean(residuals^2))
    
    subset_df_$slope <- slope
    subset_df_$intercept <- intercept
    subset_df_$r2 <- r_squared
    subset_df_$rmse <- rmse
    
    source_df <- rbind(source_df, subset_df_)
    
    cat("R-squared:", r_squared, "\n")
    cat("RMSE:", rmse, "\n")
  } 
  fitted_df <- rbind(fitted_df, source_df)
}

write.csv(fitted_df, file = paste0(path_to_data, "fitted_df_", subset_df_long$Mix[1], ".csv"),
          row.names = FALSE)

