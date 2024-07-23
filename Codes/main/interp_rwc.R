library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(purrr)
library(broom)

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
# Assuming 'df' is your dataframe and you want to remove columns named 'col1' and 'col2'
residue <- subset(residue, select = -c(EPO_Reflect, Scan))


# # R_(r,lambda) = D + E * RWC_r        (eq.10) Wang et al. 2013
# Define the RWC values for prediction
data <- residue
rwc_values <- c(0.2, 0.4, 0.6, 0.8)

# Perform operations by group
results <- data %>%
  group_by(Type, Wvl) %>%
  do({
    mod <- lm(Reflect ~ RWC, data = .)
    
    # Calculate RMSE and R-squared
    preds <- predict(mod)
    rmse <- sqrt(mean((.$Reflect - preds)^2))
    r_squared <- summary(mod)$r.squared
    
    # Create a data frame for predictions at new RWC values
    new_data <- data.frame(RWC = rwc_values, Wvl = unique(.$Wvl), Type = unique(.$Type))
    new_data$Reflect = predict(mod, newdata = data.frame(RWC = rwc_values))
    
    # Collect results
    tibble(RMSE = rmse, R2 = r_squared, Predictions = list(new_data))
  })

# new RWC ~ Reflect df
new_df <- results %>%
  pull(Predictions) %>%    # Extracts the column as a list
  bind_rows() 
new_df$Sample <- "Residue"

# merge new values with residue df
new_df <- new_df[, names(residue)]

residue_new <- rbind(residue, new_df)
write.csv(residue_new, paste0(path_to_data, "Residue_RWCinterpolated.csv"))

# Extract plot data directly
plot_data <- results %>%
  select(Type, Wvl, R2)

base_size = 14
# Create the plot
r2_plot <- ggplot(plot_data, aes(x = Wvl, y = R2, color = Type, group = Type)) +
  geom_line() +
  labs(title = "R-squared vs. Wavelength for Each Type",
       x = "Wavelength (Wvl)",
       y = "R-squared (R2)",
       color = "Type") +
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

# Display the plot
print(r2_plot)
ggsave(paste0(path_to_plots, 'rwc_inerpolation/', 'residue.png'), r2_plot, width = 10, height = 7, dpi = 300)






####################################
####################################

# Soil
soil = read.csv(paste0(path_to_data, "Soil.csv"))
soil <- soil %>%
  mutate(RWC = round(RWC, 2))
soil <- subset(soil, select = -c(EPO_Reflect, Scan))

data <- soil
library(minpack.lm)


results <- data %>%
  group_by(Type, Wvl) %>%
  do({
    initial_estimates <- list(A = coef(lm(Reflect ~ RWC, data = .))[1], B = 0, C = 0)
    
    # Starting values are important in non-linear regression to ensure convergence
    # Here are some arbitrary initial values: A=10, B=5, C=-1. Modify as necessary.
    mod <- nlsLM(Reflect ~ A + B * exp(C * RWC), data = .,
                 start = list(A = 1, B = 1, C = -1),  # Provide initial guesses for parameters
                 control = nls.lm.control(maxiter = 100))  # Increase max iterations if necessary
    
    # Calculate RMSE and R-squared
    preds <- predict(mod)
    rmse <- sqrt(mean((.$Reflect - preds)^2))
    residuals <- .$Reflect - preds
    ss_res <- sum(residuals^2)
    ss_tot <- sum((.$Reflect - mean(.$Reflect))^2)
    r_squared <- 1 - ss_res/ss_tot
    
    # Create a data frame for predictions at new RWC values
    new_data <- data.frame(RWC = rwc_values, Wvl = unique(.$Wvl), Type = unique(.$Type))
    new_data$Reflect = predict(mod, newdata = data.frame(RWC = rwc_values))
    
    # Collect results
    tibble(RMSE = rmse, R2 = r_squared, Predictions = list(new_data))
  })



# new RWC ~ Reflect df
new_df <- results %>%
  pull(Predictions) %>%    # Extracts the column as a list
  bind_rows() 
new_df$Sample <- "Soil"

# merge new values with residue df
new_df <- new_df[, names(soil)]

soil_new <- rbind(soil, new_df)
write.csv(soil_new, paste0(path_to_data, "Soil_RWCinterpolated.csv"))

# Extract plot data directly
plot_data <- results %>%
  select(Type, Wvl, R2)

base_size = 14
# Create the plot
r2_plot <- ggplot(plot_data, aes(x = Wvl, y = R2, color = Type, group = Type)) +
  geom_line() +
  labs(title = "R-squared vs. Wavelength for Each Type",
       x = "Wavelength (Wvl)",
       y = "R-squared (R2)",
       color = "Type") +
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

# Display the plot
print(r2_plot)
ggsave(paste0(path_to_plots, 'rwc_inerpolation/', 'soil.png'), r2_plot, width = 10, height = 7, dpi = 300)
