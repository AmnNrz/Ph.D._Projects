library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data = paste0("/Users/aminnorouzi/Library/CloudStorage/",
                      "OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/",
                      "Projects/Soil_Residue_Spectroscopy/Data/Spectra_shift_check/")


data <- read.csv(paste0(path_to_data, 
                                  "Spectra_shift_check.csv"),
                           header = TRUE, row.names = NULL,
                 fileEncoding = "ISO-8859-1")
data <- rename(data,  Reference = ï..Reference)

data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))

# Plot using ggplot
ggplot(data, aes(x = Peak_Valley, y = Wavelength, color = Type)) +
  geom_point() + # scatter plot points
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type") + # labels
  theme_minimal() + # a minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate x labels if needed



# Convert factors to ordered factors to ensure they plot in the order you specified
data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))

# Ensure y-axis has all unique Wavelength values as ticks
wavelength_breaks <- unique(data$Wavelength[!is.na(data$Wavelength)])

# Plot using ggplot
ggplot(data, aes(x = Peak_Valley, y = Wavelength, color = Type)) +
  geom_point() + # scatter plot points
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  scale_y_continuous(breaks = wavelength_breaks) + # all y-axis Wavelength values
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type") + # labels
  theme_minimal() + # a minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # rotate x labels if needed
        axis.text.y = element_text(angle = 0, size = 5)) # rotate y labels if needed





# Convert factors to ordered factors to ensure they plot in the order you specified
data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))

# Generate breaks every 10 units for the y-axis
min_wavelength <- min(data$Wavelength, na.rm = TRUE)
max_wavelength <- max(data$Wavelength, na.rm = TRUE)
breaks_wavelength <- seq(from = min_wavelength, to = max_wavelength, by = 50)

# Plot using ggplot
p <- ggplot(data, aes(x = Peak_Valley, y = Wavelength, color = Type)) +
  geom_point() + # scatter plot points
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  scale_y_continuous(breaks = breaks_wavelength) + # breaks every 10 units
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type") + # labels
  theme_minimal() + # a minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # rotate x labels and make them smaller
        axis.text.y = element_text(angle = 0, size = 8), # make y labels horizontal and smaller
        legend.position = "bottom") # move legend to the bottom

# Display the plot
print(p)



# Convert factors to ordered factors to ensure they plot in the order you specified
data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))
data$Reference <- factor(data$Reference) # make sure Reference is a factor for the shape mapping

# Generate breaks every 10 units for the y-axis
min_wavelength <- min(data$Wavelength, na.rm = TRUE)
max_wavelength <- max(data$Wavelength, na.rm = TRUE)
breaks_wavelength <- seq(from = min_wavelength, to = max_wavelength, by = 30)

# Plot using ggplot
p <- ggplot(data, aes(x = Peak_Valley, y = Wavelength, color = Type, shape = Reference)) +
  geom_point() + # scatter plot points
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  scale_y_continuous(breaks = breaks_wavelength) + # breaks every 10 units
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type", shape = "Reference") + # labels
  theme_minimal() + # a minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # rotate x labels and make them smaller
        axis.text.y = element_text(angle = 0, size = 8), # make y labels horizontal and smaller
        legend.position = "bottom") # move legend to the bottom

# Display the plot
print(p)


# Convert factors to ordered factors to ensure they plot in the order you specified
data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))
data$Reference <- factor(data$Reference) # make sure Reference is a factor for the shape mapping

# Generate breaks every 10 units for the y-axis
min_wavelength <- min(data$Wavelength, na.rm = TRUE)
max_wavelength <- max(data$Wavelength, na.rm = TRUE)
breaks_wavelength <- seq(from = min_wavelength, to = max_wavelength, by = 30)

# Plot using ggplot
p <- ggplot(data, aes(x = Peak_Valley, y = Wavelength, color = Type, shape = Reference)) +
  geom_point() + # scatter plot points
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  scale_y_continuous(breaks = breaks_wavelength) + # breaks every 10 units
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type", shape = "Reference") + # labels
  theme_minimal() + # a minimal theme
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # rotate x labels and make them smaller
    axis.text.y = element_text(angle = 0, size = 8), # make y labels horizontal and smaller
    legend.position = "right" # position legends on the right
  )

# Display the plot
print(p)



# Convert factors to ordered factors to ensure they plot in the order you specified
data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))
data$Reference <- factor(data$Reference) # make sure Reference is a factor for the shape mapping

# Ensure y-axis has all unique Wavelength values as ticks
wavelength_breaks <- unique(data$Wavelength[!is.na(data$Wavelength)])

# Plot using ggplot
p <- ggplot(data, aes(x = Peak_Valley, y = Wavelength, color = Type, shape = Reference)) +
  geom_point() + # scatter plot points
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  scale_y_continuous(breaks = wavelength_breaks) + # actual y-axis Wavelength values
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type", shape = "Reference") + # labels
  theme_minimal() + # a minimal theme
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # rotate x labels and make them smaller
    axis.text.y = element_text(angle = 0, size = 8), # make y labels horizontal and smaller
    legend.position = "right" # position legends on the right
  )

# Display the plot
print(p)




# Convert factors to ordered factors to ensure they plot in the order you specified
data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))
data$Reference <- factor(data$Reference) # make sure Reference is a factor for the shape mapping

# Compute min and max wavelength for each Peak_Valley
wavelength_stats <- data %>%
  group_by(Peak_Valley) %>%
  summarise(min_wavelength = min(Wavelength, na.rm = TRUE),
            max_wavelength = max(Wavelength, na.rm = TRUE)) %>%
  ungroup()

# Combine min and max wavelengths into a single vector
wavelength_breaks <- unique(c(wavelength_stats$min_wavelength, wavelength_stats$max_wavelength))

# Plot using ggplot
p <- ggplot(data, aes(x = Peak_Valley, y = Wavelength, color = Type, shape = Reference)) +
  geom_point(size = 4, alpha = 0.2) + # scatter plot points
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  scale_y_continuous(breaks = wavelength_breaks) + # min and max y-axis Wavelength values
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type", shape = "Reference") + # labels
  theme_minimal() + # a minimal theme
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # rotate x labels and make them smaller
    axis.text.y = element_text(angle = 0, size = 8), # make y labels horizontal and smaller
    legend.position = "right" # position legends on the right
  )

# Display the plot
print(p)






data$Peak_Valley <- factor(data$Peak_Valley, levels = c('Peak 1', 'Valley 1', 'Peak 2', 'Valley 2', 'Peak 3', 'Valley 3', 'Peak 4', 'Valley 4', 'Peak 5', 'Valley 5', 'Peak 6', 'Valley 6'))
data$Reference <- factor(data$Reference) # make sure Reference is a factor for the shape mapping

# Compute min and max wavelength for each Peak_Valley
wavelength_stats <- data %>%
  group_by(Peak_Valley) %>%
  summarise(min_wavelength = min(Wavelength, na.rm = TRUE),
            max_wavelength = max(Wavelength, na.rm = TRUE)) %>%
  ungroup()

# Combine min and max wavelengths into a single vector
wavelength_breaks <- unique(c(wavelength_stats$min_wavelength, wavelength_stats$max_wavelength))

# Define a set of shapes
shapes <- c(16, 17, 18, 19, 15) # Filled circle, triangle, square, diamond, and filled square

# Plot using ggplot
p <- ggplot(data, aes(x = Wavelength, y = Peak_Valley, color = Type, shape = Reference)) +
  geom_point(size = 3, alpha = 0.4) + # slightly larger and transparent points
  scale_shape_manual(values = shapes) + # manually set shapes
  scale_x_discrete(limits = levels(data$Peak_Valley)) + # ensure x-axis order
  scale_y_continuous(breaks = wavelength_breaks) + # min and max y-axis Wavelength values
  labs(x = "Peak/Valley", y = "Wavelength (nm)", color = "Type", shape = "Reference") + # labels
  theme_minimal() + # a minimal theme
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # rotate x labels and make them smaller
    axis.text.y = element_text(angle = 0, size = 8), # make y labels horizontal and smaller
    legend.position = "right" # position legends on the right
  )

p <- ggplot(data, aes(x = Wavelength, y = Peak_Valley, color = Type, shape = Reference)) + 
  geom_point(size = 3, alpha = 0.4) + # slightly larger and transparent points
  scale_x_continuous(breaks = wavelength_breaks) + # Adjusted for the new x-axis
  scale_y_discrete(limits = levels(data$Peak_Valley)) + # Adjusted for the new y-axis
  labs(x = "Wavelength (nm)", y = "Peak/Valley", color = "Type", shape = "Reference") + # Adjusted labels
  theme_minimal() + # a minimal theme
  theme(
    axis.text.x = element_text(angle = 0, size = 8), # Adjusted for the new x-axis (no rotation needed)
    axis.text.y = element_text(angle = 90, hjust = 1, size = 8), # rotate y labels and make them smaller
    legend.position = "right" # position legends on the right
  )


# Display the plot
print(p)

