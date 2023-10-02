library(ggplot2)
library(viridis)
library(reshape2)
library(dplyr)
library(gridExtra)

# # On my Mac
path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Data/')
path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Spectroscopy_Paper/Plots/')

# On Ubuntu
# path = "/home/amnnrz/GoogleDrive - msaminnorouzi/PhD/Projects/Spectroscopy paper/EPO/"


# WheatDuet_original <- read.csv("wheatDuet_sperctra_original.csv")
# ## On my Ubuntu
# setwd('/home/amnnrz/GoogleDrive - msaminnorouzi/PhD/Projects/Spectroscopy paper/EPO/')
# WheatDuet_original <- read.csv("wheatDuet_sperctra_original.csv")

# # Delete rows where Scan is 0.16
# # WheatDuet_original <- WheatDuet_original[WheatDuet_original$Scan != 0.16, ]
# WheatDuet_original$Source <- 'Original' # Add a source column
# 
# WheatDuet_EPO <- read.csv("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/WheatDuet_spectra_EpoApplied.csv")
# WheatDuet_EPO$Source <- 'EPO' # Add a source column
# 
# WheatDuet_original <- WheatDuet_original[c("X", "Wvl", "Scan", "Reflect", "Scan", "Source" )]
# 
# 
# # Combine the two data frames
# WheatDuet_combined <- rbind(WheatDuet_EPO, WheatDuet_original)
# 
# # Get all unique Scan levels from the combined dataframes
# all_Scan_levels <- unique(WheatDuet_combined$Scan)
# 
# # Create a color palette with enough colors for all Scan levels
# custom_colors <- viridis(length(all_Scan_levels))
# 
# 
# # # Define your custom_colors vector
# # custom_colors <- c("red", "blue")  # Change these to your desired colors
# 
# 
# # Assuming df is your dataframe and 'column_name' is the name of the column
# # WheatDuet_combined <- WheatDuet_combined %>% filter(Scan != 1.12)
# 
# WheatDuet_combined$Source <- factor(WheatDuet_combined$Source, levels =c("Original", "EPO"), order=TRUE)
# 
# # Plot code
# p <- ggplot(WheatDuet_combined, aes(Wvl, Reflect, group = factor(Scan))) +
#   geom_line(aes(color = factor(Scan))) +
#   labs(title = paste('WheatDuet'), x = "Wavelength(nm)", y = "Reflectance", 
#        color = "Scan Levels") +
#   scale_color_manual(values = custom_colors, 
#                      name = "Scan levels") +  # Add legend title here
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_line(colour = "gray90"),
#     panel.grid.minor = element_blank(),
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14),
#     plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
#     legend.title = element_text(size = 12),  # Customize legend title appearance
#     legend.text = element_text(size = 12)
#   ) +
#   guides(color = guide_legend(override.aes = list(shape = NA))) +
#   facet_wrap(~ Source, scales = "free_y") # Facet the plots by the source with free y-axis scales
# 
# # p <- p + ggtitle("Your Plot Title")
# # Print the plot
# print(p)

###################################################
###################################################
              # Plot for all crops #
###################################################
###################################################
library(patchwork)

Crop_original <- read.csv(paste0(path_to_data, 
                     "Updated_data_2_10nm_res/soil_original.csv"), header = TRUE, row.names = NULL)

Crop_original$Source <- 'Original' # Add a source column

Crop_EPO <-  read.csv(paste0(path_to_data, 
                   "Updated_data_2_10nm_res/allSoils_EPO.csv"), header = TRUE, row.names = NULL)
Crop_EPO$Source <- 'EPO' # Add a source column

colnames(Crop_EPO)[colnames(Crop_EPO) == "Crop"] <- "Type_Name"

Crop_EPO <- Crop_EPO %>%
  select(names(Crop_original))

Crop_combined <- rbind(Crop_EPO, Crop_original)

Crop_combined$Sample <- 'All Crops'

# remove scan1
Crop_combined <- Crop_combined[Crop_combined$Scan != 'Scan1',]

plots_list <- list()

crp = 'All Soils'

print(crp)

# Get all unique Scan levels from the combined dataframes
all_Scan_levels <- unique(Crop_combined$Scan)

# Create a color palette with enough colors for all Scan levels
custom_colors <- viridis(length(all_Scan_levels))
custom_colors <- rev(custom_colors)

Crop_combined$Source <- factor(Crop_combined$Source, levels =c("Original", "EPO"), order=TRUE)

Crop_combined <- Crop_combined %>%
  mutate(Source = recode(Source,"Original"="Original Spectra", "EPO"="EPO Spectra"
                            ))



# Main plot
p <- ggplot(Crop_combined, aes(Wvl, Reflect, group = factor(Scan))) +
  geom_line(aes(color = factor(Scan))) +
  labs(title = paste(crp),
       x = "Wavelength(nm)", y = "Reflectance",
       color = "Scan Levels") +
  scale_color_manual(values = custom_colors,
                     name = "Scan levels") +  # Add legend title here
  theme_minimal() +
  theme(
    # panel.grid.major = element_line(colour = "gray90"),
    # panel.grid.minor = element_blank(),

    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),           # Remove major grid lines
    panel.grid.minor = element_blank(),           # Remove minor grid lines
    strip.background = element_rect(fill = "white"), # Remove facet strip background

    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "plain"),
    legend.title = element_text(size = 12),  # Customize legend title appearance
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(override.aes = list(shape = NA))) +
  facet_wrap(~ Source, scales = "free_y") # Facet the plots by the source with free y-axis scales

# Print the plot
print(p)

ggsave(filename = paste0(path_to_plots,
                         "All_crops_soils_merged/Before&After_Epo_reflect_without_scan1_allSoils.png"),
       plot = p, width = 8.3, height = 5.8, dpi = 200)
