library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

# path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/Wangetal2')

residue = read.csv(paste0(path_to_data, "Residue.csv"))

