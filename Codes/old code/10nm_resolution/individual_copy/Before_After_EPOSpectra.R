library(ggplot2)
library(viridis)
library(reshape2)
library(dplyr)
library(gridExtra)
library(patchwork)


###################################################
###################################################
              # Plot for all crops #
###################################################
###################################################

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/Before_After_epo/')


Residue_Median <- read.csv(paste0(path_to_data, 
                                  "Residue.csv"),
                           header = TRUE, row.names = NULL)
Crop_original <- Residue_Median[-c(1, 8)]

Crop_original <- rename(Crop_original, Crop = Soil)

Crop_original <- Crop_original %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))

Crop_original$Source <- 'Original' # Add a source column
Crop_original <- Crop_original %>% 
  rename(Type = Crop)

Crop_EPO <- read.csv(paste0(path_to_data, 'Residue_Transformed.csv'), 
                     header = TRUE, row.names = NULL)
Crop_EPO$Source <- 'EPO' # Add a source column
Crop_EPO <- Crop_EPO %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))

Crop_EPO <- Crop_EPO %>%
  select(names(Crop_original))

Crop_combined <- rbind(Crop_EPO, Crop_original)

###########################################################################
###########################################################################
###############                                             ###############
###############          PLOT THE DRIEST REFLECTANCE        ###############
###############                                             ###############
###########################################################################
###########################################################################


original_driest_df <- data.frame()
for (crp in unique(Crop_original$Type)){
  crp_df <- Crop_original %>% dplyr:: filter(Type == crp)
  driest <-   crp_df %>%  dplyr:: filter(RWC == min(crp_df$RWC))
  original_driest_df <- rbind(original_driest_df, driest)
}

epo_driest_df <- data.frame()
for (crp in unique(Crop_EPO$Type)){
  crp_df <- Crop_EPO %>% dplyr:: filter(Type == crp)
  driest <-   crp_df %>%  dplyr:: filter(RWC == min(crp_df$RWC))
  epo_driest_df <- rbind(epo_driest_df, driest)
}

driest_combined <- rbind(original_driest_df, epo_driest_df)


library(patchwork)

# Get all unique RWC levels from the combined dataframes
crop_levels <- unique(driest_combined$Type)

# Create a color palette with enough colors for all RWC levels
custom_colors <- viridis(length(crop_levels))
custom_colors <- rev(custom_colors)

driest_combined$Source <- factor(driest_combined$Source, levels =c("Original", "EPO"), order=TRUE)

driest_combined <- driest_combined %>%
  mutate(Source = recode(Source,"Original"="Original Spectra", "EPO"="EPO Spectra"
  ))

list_of_dataframes <- split(driest_combined, driest_combined$Source)
list_of_plots <- lapply(list_of_dataframes, function(df) {
  
  
  # Calculate the y-breaks specific to this dataframe
  y_breaks <- seq(min(df$Reflect), max(df$Reflect), by = (max(df$Reflect) - min(df$Reflect))/20)
  # print(y_breaks)
  p <- ggplot(df, aes(Wvl, Reflect, group = factor(Type))) +
    geom_line(aes(color = factor(Type))) +
    geom_vline(xintercept = c(2000, 2100, 2200), linetype = "solid", size = 0.6, color = 'red') +  # Add vertical lines
    geom_vline(xintercept = c(1660, 2330), linetype = "solid", size = 0.6, color = 'blue') +  # Add vertical lines
    geom_vline(xintercept = c(2205, 2260), linetype = "solid", size = 0.6, color = 'green') +  # Add vertical lines
    labs(title = paste0(df$Source),
         x = "Wavelength(nm)", y = "Reflectance",
         color = "Crops") +
    scale_x_continuous(breaks = seq(min(driest_combined$Wvl), max(driest_combined$Wvl), by = 200)) +  # x-axis grid lines every 50 units
    scale_y_continuous(breaks = y_breaks) +  # custom y-axis grid lines
    scale_color_manual(values = custom_colors,
                       name = "Crops") +  # Add legend title here
    theme_minimal() +
    theme(
      # panel.grid.major = element_line(colour = "gray90"),
      # panel.grid.minor = element_blank(),
      
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "gray90"),           # Remove major grid lines
      panel.grid.minor = element_line(colour = "gray90"),           # Remove minor grid lines
      strip.background = element_rect(fill = "white"), # Remove facet strip background
      
      axis.text = element_text(size = 12),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.title = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
      legend.title = element_text(size = 12),  # Customize legend title appearance
      legend.text = element_text(size = 12)
    ) 
  guides(color = guide_legend(override.aes = list(shape = NA)))
  # facet_wrap(~ Source, scales = "free_y") # Facet the plots by the source with free y-axis scales
  
  print(p)
})
combined_plot <- wrap_plots(list_of_plots)
print(combined_plot)

ggsave(paste0(path_to_plots, "driest_reflects.png"), combined_plot,
       width = 45, height = 15, units = "cm",dpi = 200)

###########################################################################
###########################################################################
###########################################################################
###########################################################################

# Crop_combined <- Crop_combined %>%
  # mutate(Type = recode(Type,"Almira_bottom"="Almira (bottom)", "Almira_top"="Almira (top)",
                                   # "Bickleton_bottom"="Bickleton (bottom)", "Bickleton_top"="Bickleton (top)",
                                   # "Lind_bottom"="Lind (bottom)", "Lind_top"="Lind (top)",
                                   # "Palouse_conv_till"="Palouse (conventional till)", 
                                   # "Palouse_no_till"="Palouse (no till)", "Pomeroy_bottom"="Pomeroy (bottom)",
                                   # "Pomeroy_top"="Pomeroy (top)", "Wike_bottom"="Wike (bottom)", "Wike_top"="Wike (top)" ))

# # Initialize a patchwork object to hold all plots
# all_plots <- NULL
plots_list <- list()
# crp <- unique(Crop_combined$Type)[1]
for (crp in unique(Crop_combined$Type)){
  # Combine the two data frames
  Crop_combined <- rbind(Crop_EPO, Crop_original)
  # Crop_combined <- Crop_combined %>%
  #   mutate(Type = recode(Type,"Almira_bottom"="Almira (bottom)", "Almira_top"="Almira (top)",
  #                           "Bickleton_bottom"="Bickleton (bottom)", "Bickleton_top"="Bickleton (top)",
  #                           "Lind_bottom"="Lind (bottom)", "Lind_top"="Lind (top)",
  #                           "Palouse_conv_till"="Palouse (conventional till)", 
  #                           "Palouse_no_till"="Palouse (no till)", "Pomeroy_bottom"="Pomeroy (bottom)",
  #                           "Pomeroy_top"="Pomeroy (top)", "Wike_bottom"="Wike (bottom)", "Wike_top"="Wike (top)" ))
  # 
  print(crp)
  Crop_combined <- subset(Crop_combined, Type == crp)
  
  # Get all unique RWC levels from the combined dataframes
  all_rwc_levels <- unique(Crop_combined$RWC)
  
  # Create a color palette with enough colors for all RWC levels
  custom_colors <- viridis(length(all_rwc_levels))
  custom_colors <- rev(custom_colors)
  
  Crop_combined$Source <- factor(Crop_combined$Source, levels =c("Original", "EPO"), order=TRUE)
  
  Crop_combined <- Crop_combined %>%
    mutate(Source = recode(Source,"Original"="Original Spectra", "EPO"="EPO Spectra"
                              ))
  
 
  
  # # Main plot
  # p <- ggplot(Crop_combined, aes(Wvl, Reflect, group = factor(RWC))) +
  #   geom_line(aes(color = factor(RWC))) +
  #   labs(title = paste(crp),
  #        x = "Wavelength(nm)", y = "Reflectance",
  #        color = "RWC Levels") +
  #   scale_color_manual(values = custom_colors,
  #                      name = "RWC levels") +  # Add legend title here
  #   theme_minimal() +
  #   theme(
  #     # panel.grid.major = element_line(colour = "gray90"),
  #     # panel.grid.minor = element_blank(),
  # 
  #     panel.background = element_rect(fill = "white"),
  #     plot.background = element_rect(fill = "white"),
  #     panel.grid.major = element_blank(),           # Remove major grid lines
  #     panel.grid.minor = element_blank(),           # Remove minor grid lines
  #     strip.background = element_rect(fill = "white"), # Remove facet strip background
  # 
  #     axis.text = element_text(size = 12),
  #     axis.title = element_text(size = 14),
  #     plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
  #     legend.title = element_text(size = 12),  # Customize legend title appearance
  #     legend.text = element_text(size = 12)
  #   ) +
  #   guides(color = guide_legend(override.aes = list(shape = NA))) +
  #   facet_wrap(~ Source, scales = "free_y") # Facet the plots by the source with free y-axis scales
  # 
  # # Print the plot
  # print(p)
  # 
  # 
  
  library(patchwork)
  
  list_of_dataframes <- split(Crop_combined, Crop_combined$Source)
  list_of_plots <- lapply(list_of_dataframes, function(df) {
    
    
  # Calculate the y-breaks specific to this dataframe
    y_breaks <- seq(min(df$Reflect), max(df$Reflect), by = (max(df$Reflect) - min(df$Reflect))/20)
    # print(y_breaks)
  p <- ggplot(df, aes(Wvl, Reflect, group = factor(RWC))) +
    geom_line(aes(color = factor(RWC))) +
    geom_vline(xintercept = c(2000, 2100, 2200), linetype = "solid", size = 0.6, color = 'red') +  # Add vertical lines
    geom_vline(xintercept = c(1660, 2330), linetype = "solid", size = 0.6, color = 'blue') +  # Add vertical lines
    geom_vline(xintercept = c(2205, 2260), linetype = "solid", size = 0.6, color = 'green') +  # Add vertical lines
    labs(title = paste0(df$Source, " ", "(", crp, ")"),
         x = "Wavelength(nm)", y = "Reflectance",
         color = "RWC Levels") +
    scale_x_continuous(breaks = seq(min(Crop_combined$Wvl), max(Crop_combined$Wvl), by = 100),
                       minor_breaks = seq(min(df$Wvl), max(df$Wvl), by = 10)) +  # x-axis grid lines every 50 units
    scale_y_continuous(breaks = y_breaks,
                       labels = scales::label_number(scale = 1, accuracy = 0.01)
                       ) +  # custom y-axis grid lines
    scale_color_manual(values = custom_colors,
                       name = "RWC levels") +  # Add legend title here
    theme_minimal() +
    theme(
      # panel.grid.major = element_line(colour = "gray90"),
      # panel.grid.minor = element_blank(),
      
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "gray90"),           # Remove major grid lines
      panel.grid.minor = element_line(colour = "gray90"),           # Remove minor grid lines
      strip.background = element_rect(fill = "white"), # Remove facet strip background
      
      axis.text = element_text(size = 12),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.title = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
      legend.title = element_text(size = 12),  # Customize legend title appearance
      legend.text = element_text(size = 12)
    ) 
    guides(color = guide_legend(override.aes = list(shape = NA)))
    # facet_wrap(~ Source, scales = "free_y") # Facet the plots by the source with free y-axis scales
    
    print(p)
    # ggsave(paste0(path_to_plots, crp, ".png"), p,
    #        width = 45, height = 15, units = "cm",dpi = 200)
  })
  
  combined_plot <- wrap_plots(list_of_plots)
  print(combined_plot)
  
  # # Main plot
  # p <- ggplot(Crop_combined, aes(Wvl, Reflect, group = factor(RWC))) +
  #   geom_line(aes(color = factor(RWC))) +
  #   geom_vline(xintercept = c(2000, 2100, 2200), linetype = "solid", size = 0.6, color = 'red') +  # Add vertical lines
  #   geom_vline(xintercept = c(1660, 2330), linetype = "solid", size = 0.6, color = 'blue') +  # Add vertical lines
  #   geom_vline(xintercept = c(2205, 2260), linetype = "solid", size = 0.6, color = 'green') +  # Add vertical lines
  #   labs(title = paste(crp),
  #        x = "Wavelength(nm)", y = "Reflectance",
  #        color = "RWC Levels") +
  #   scale_x_continuous(breaks = seq(min(Crop_combined$Wvl), max(Crop_combined$Wvl), by = 200)) +  # x-axis grid lines every 50 units
  #   scale_y_continuous(breaks = calculate_breaks) +  # custom y-axis grid lines
  #   scale_color_manual(values = custom_colors,
  #                      name = "RWC levels") +  # Add legend title here
  #   theme_minimal() +
  #   theme(
  #     # panel.grid.major = element_line(colour = "gray90"),
  #     # panel.grid.minor = element_blank(),
  #     
  #     panel.background = element_rect(fill = "white"),
  #     plot.background = element_rect(fill = "white"),
  #     panel.grid.major = element_blank(),           # Remove major grid lines
  #     panel.grid.minor = element_blank(),           # Remove minor grid lines
  #     strip.background = element_rect(fill = "white"), # Remove facet strip background
  #     
  #     axis.text = element_text(size = 12),
  #     axis.text.x = element_text(size = 32, angle = 45, hjust = 1),
  #     axis.title = element_text(size = 14),
  #     plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
  #     legend.title = element_text(size = 12),  # Customize legend title appearance
  #     legend.text = element_text(size = 12)
  #   ) +
  #   guides(color = guide_legend(override.aes = list(shape = NA))) +
  #   facet_wrap(~ Source, scales = "free_y") # Facet the plots by the source with free y-axis scales
  # 
  # # Print the plot
  # print(p)
  # 
  # 
  
  
  
  
  
  
  #########################################################
  # Crop_combined <- Crop_combined %>%  dplyr:: filter(Source == 'Original Spectra')
  # Crop_combined <- Crop_combined %>%  dplyr:: filter(RWC == )
  
  # # Main plot
  # calculate_breaks <- function(limits) {
  #   seq(floor(limits[1]), ceiling(limits[2]), by = 5)  # Adjust 0.1 to whatever increment you want
  # }
  # 
  # 
  # Crop_combined <- Crop_combined %>% filter(Scan!= "Scan2")
  # p <- ggplot(Crop_combined, aes(x = Wvl, y = Reflect, group = factor(RWC))) +
  #   geom_line(aes(color = factor(RWC)),linewidth = 3) +
  #   geom_vline(xintercept = c(2000, 2100, 2200), linetype = "solid", size = 0.6, color = 'red') +  # Add vertical lines
  #   geom_vline(xintercept = c(1660, 2330), linetype = "solid", size = 0.6, color = 'blue') +  # Add vertical lines
  #   geom_vline(xintercept = c(2205, 2260), linetype = "solid", size = 0.6, color = 'green') +  # Add vertical lines
  #   labs(title = paste(crp),
  #        x = "Wavelength(nm)", y = "Reflectance",
  #        color = "RWC Levels") +
  #   scale_x_continuous(breaks = seq(min(Crop_combined$Wvl), max(Crop_combined$Wvl), by = 100)) +  # x-axis grid lines every 50 units
  #   scale_y_continuous(breaks = calculate_breaks) +  # custom y-axis grid lines
  # 
  #   scale_color_manual(values = custom_colors,
  #                      name = "RWC levels") +  # Add legend title here
  #   theme_minimal() +
  #   theme(
  #     panel.background = element_rect(fill = "white"),
  #     plot.background = element_rect(fill = "white"),
  #     panel.grid.major = element_line(colour = "#878787"),
  #     panel.grid.minor = element_line(colour = "#878787"),
  #     strip.background = element_rect(fill = "white"), # Remove facet strip background
  # 
  #     axis.text = element_text(size = 32),
  #     axis.text.x = element_text(size = 32, angle = 45, hjust = 1),
  #     axis.title = element_text(size = 42),
  #     plot.title = element_text(hjust = 0.5, size = 42, face = "plain"),
  #     strip.text = element_text(size = 32, face = "bold"),  # Adjust the number and face as needed 
  #     legend.title = element_text(size = 32),  # Customize legend title appearance
  #     legend.text = element_text(size = 32)
  #   ) +
  #   guides(color = guide_legend(override.aes = list(shape = NA))) +
  #   facet_wrap(~ Source, scales = "free_y") # Facet the plots by the source with free y-axis scales

  # Print the plot
  # print(p)
  
  
  # plots_list[[crp]] <- p
  # Save the figure as a PDF with A5 size (width = 14.8 cm, height = 21 cm)
  ggsave(paste0(path_to_plots, crp, "_org_epo.png"), combined_plot,
         width = 45, height = 15, units = "cm",dpi = 200)
  # 
}
# # Print or save the final plot assembly
# # Combine plots into a grid with wrap_plots
# all_plots <- wrap_plots(plots_list, ncol = 2)  # Adjust 'ncol' to your preference

# Print or save the final plot assembly
# print(all_plots)
# setwd("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/plots/Before&After_EPO_Spectra/")
# ggsave("6_Soils.png", width = 45, height = 40, units = "cm",dpi = 200)


