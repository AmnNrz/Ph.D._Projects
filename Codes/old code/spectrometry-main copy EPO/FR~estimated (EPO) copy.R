# Load the reshape2 package
library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(minpack.lm)


# Set the path to directory
# Set the path to directory
# path_to_directory <- paste0("/Users/aminnorouzi/Library/CloudStorage/",
#                       "GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/",
#                       "Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl")

path_to_directory <- paste0("/home/amnnrz/GoogleDrive - msaminnorouzi/PhD/",
                            "Projects/Spectroscopy paper/Spectrometry-main copy EPO/",
                            "index_org_trsfed_crp_sl/")


# Get a list of all .csv files in the directory
all_csv_files <- list.files(path = path_to_directory, pattern = "\\.csv$", full.names = FALSE)

# Remove the ".csv" extension from the file names
csv_file_fullname <- sub("\\.csv$", "", all_csv_files)

# Extract characters after the second underscore
crop_soil_names <- sapply(strsplit(csv_file_fullname, "_"), function(x) paste(tail(x, -2), collapse = "_"))
print(unique(crop_soil_names))

crops <- unique(sapply(strsplit(crop_soil_names, "_"), function(x) paste(x[1])))
soils <- unique(sapply(strsplit(crop_soil_names, "_"), function(x) paste(x[2])))

# setwd(paste0("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive",
#               "-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper",
#               "/Spectrometry-main copy EPO"))

CROP = "Peas"
################################################################
##################          NDTI            ####################
################################################################

df_original <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0(path_to_directory, 'estAFTER_EPO/NDTI_Original_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_original <- rbind(df_original, index_df)
  
}

df_original_minRWC <- df_original %>%
  group_by(soil) %>%
  mutate(min_rwc = min(RWC.y)) %>%  # Calculate the min RWC.y for each soil type
  filter(RWC.y == min_rwc) %>%      # Keep rows where RWC.y equals the minimum
  ungroup() 

df_original_minRWC$Slope = NA
df_original_minRWC$Intercept = NA


# Perform linear regression (Fr ~ index)
for (rwc in unique(df_original_minRWC$RWC.y)){
  df <- dplyr::filter(df_original_minRWC, RWC.y == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_original_minRWC <- df_original_minRWC %>%
    mutate(Slope = ifelse(RWC.y == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.y == rwc, intercept, Intercept))
} 

df_original$Slope_base <- df_original_minRWC$Slope[1]
df_original$Intercept_base <- df_original_minRWC$Intercept[1]
df_original$Fr_est <- df_original$Intercept_base + 
  df_original$Slope_base * df_original$CAI

# Load the dplyr package
library(dplyr)

# plot fr_act ~fr_est
df_original <- df_original %>%
  mutate(
    rwc_range = case_when(
      RWC.y < 0.25  ~ "RWC < 25%",
      RWC.y >= 0.25 & RWC.y <= 0.75 ~ "25% < RWC < 75%",
      RWC.y > 0.75  ~ "RWC > 75%",
      TRUE ~ "Other" # This will catch any NA or other unexpected values
    )
  )


df_original$rwc_range <- factor(df_original$rwc_range, levels = c("RWC < 25%", "25% < RWC < 75%", "RWC > 75%"))

# Create the scatter plot
p1 <- ggplot(df_original, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
  labs(title = "NDTI - Peas",
    x = "Fraction residue cover", y ="Estimated fraction residue cover") +
  theme(legend.title = "RWC",
        plot.title = element_text(hjust = 0.5))+
  theme_minimal() +
  # scale_color_viridis(discrete = TRUE, option = "E") +
  scale_x_continuous(limits = c(0, 1)) + # Change as needed
  scale_y_continuous(limits = c(0, 1)) + # Change as needed
  coord_fixed(ratio = 1)
print(p1)

lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_original)

# Add a new column to df_original to store the predicted values
df_original$Predictions <- NA  # Initialize with NA

# Populate this column with the predicted values
df_original$Predictions[!is.na(df_original$Fr_est)] <- predict(lm_model, newdata = df_original[!is.na(df_original$Fr_est),])

# Calculate residuals and RMSE only for the rows for which predictions were actually made
valid_rows <- !is.na(df_original$Predictions) & !is.na(df_original$Fraction_Residue_Cover)

# Calculate residuals
residuals <- df_original$Fraction_Residue_Cover[valid_rows] - df_original$Predictions[valid_rows]

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))

df_original$INDEX <- 'NDTI'
df_original$Method <- 'After EPO'
df_original_NDTI <- df_original


##########################################################################
######################        Fr_es after EPO       ######################
##########################################################################


df_transformed <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0(path_to_directory, 'estAFTER_EPO/NDTI_transformed_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_transformed <- rbind(df_transformed, index_df)
  
}

df_transformed_minRWC <- df_transformed %>%
  group_by(soil) %>%
  mutate(min_rwc = min(RWC.y)) %>%  # Calculate the min RWC.y for each soil type
  filter(RWC.y == min_rwc) %>%      # Keep rows where RWC.y equals the minimum
  ungroup() 

df_transformed_minRWC$Slope = NA
df_transformed_minRWC$Intercept = NA

# Perform linear regression (Fr ~ index)
for (rwc in unique(df_transformed_minRWC$RWC.y)){
  df <- dplyr::filter(df_transformed_minRWC, RWC.y == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_transformed_minRWC <- df_transformed_minRWC %>%
    mutate(Slope = ifelse(RWC.y == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.y == rwc, intercept, Intercept))
} 

df_transformed$Slope_base <- df_transformed_minRWC$Slope[1]
df_transformed$Intercept_base <- df_transformed_minRWC$Intercept[1]
df_transformed$Fr_est_transformedBaseline <- df_transformed$Intercept_base + 
  df_transformed$Slope_base * df_transformed$CAI

df_original_wet <- df_original %>%
  group_by(soil) %>%
  mutate(min_rwc = min(RWC.y)) %>%  # Calculate the min RWC.y for each soil type
  filter(RWC.y != min_rwc) %>%      # remove rows where RWC.y equals the minimum
  ungroup() 

df_transformed$Fr_est_originalBaseline <- df_original_wet$Intercept_base + 
  df_original_wet$Slope_base * df_transformed$CAI

# Load the dplyr package
library(dplyr)

# plot fr_act ~fr_est
df_transformed <- df_transformed %>%
  mutate(
    rwc_range = case_when(
      RWC.y < 0.25  ~ "RWC < 25%",
      RWC.y >= 0.25 & RWC.y <= 0.75 ~ "25% < RWC < 75%",
      RWC.y > 0.75  ~ "RWC > 75%",
      TRUE ~ "Other" # This will catch any NA or other unexpected values
    )
  )


df_transformed$rwc_range <- factor(df_transformed$rwc_range, levels = c("RWC < 25%", "25% < RWC < 75%", "RWC > 75%"))

# Create the scatter plot
p1 <- ggplot(df_transformed, aes(y = Fr_est_originalBaseline, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
  labs(title = "NDTI - Peas",
       x = "Fraction residue cover", y ="Estimated fraction residue cover") +
  theme(legend.title = "RWC",
        plot.title = element_text(hjust = 0.5))+
  theme_minimal() +
  # scale_color_viridis(discrete = TRUE, option = "E") +
  scale_x_continuous(limits = c(0, 1)) + # Change as needed
  scale_y_continuous(limits = c(0, 1)) + # Change as needed
  coord_fixed(ratio = 1)
print(p1)

lm_model <- lm(Fraction_Residue_Cover ~ Fr_est_originalBaseline, data = df_transformed)

# Add a new column to df_original to store the predicted values
df_transformed$Predictions <- NA  # Initialize with NA

# Populate this column with the predicted values
df_transformed$Predictions[!is.na(df_transformed$Fr_est_originalBaseline)] <- predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est_originalBaseline),])

# Calculate residuals and RMSE only for the rows for which predictions were actually made
valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)

# Calculate residuals
residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))

df_transformed$INDEX <- 'NDTI'
df_transformed$Method <- 'After EPO'
df_transformed_NDTI <- df_transformed



################################################################
##################           CAI          ######################
################################################################

df_original <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0(path_to_directory, 'estAFTER_EPO/SINDRI_transformed_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_original <- rbind(df_original, index_df)
  
}

df_original$Slope = NA
df_original$Intercept = NA


# Perform linear regression (Fr ~ index)
for (rwc in unique(df_original$RWC.y)){
  df <- dplyr::filter(df_original, RWC.y == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_original <- df_original %>%
    mutate(Slope = ifelse(RWC.y == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.y == rwc, intercept, Intercept))
}  

df_original$A_slope = NA
df_original$B_slope = NA
df_original$C_slope = NA

for (rwc in unique(df_original$RWC.y)){
  ####
  # Fit exponential curve to Slope~RWC
  
  ###########
  # Slope
  ###########
  # Initial parameter guesses
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Slope <- unique(df_original[, c("RWC.y", "Slope")])
  
  fit <- nlsLM(RWC_Slope$Slope ~ a + b * exp(c * RWC_Slope$RWC.y),
               data = RWC_Slope, start = initial_guess)
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_original <- df_original %>%
    mutate(A_slope = ifelse(RWC.y == rwc, a, A_slope)) %>%
    mutate(B_slope = ifelse(RWC.y == rwc, b, B_slope)) %>%
    mutate(C_slope = ifelse(RWC.y == rwc, c, C_slope))
}



#   ###########
#   # Intercept
#   ###########

df_original$A_intcpt = NA
df_original$B_intcpt = NA
df_original$C_intcpt = NA

for (rwc in unique(df_original$RWC.y)){
  
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Intercept <- unique(df_original[, c("RWC.y", "Intercept")])
  
  fit <- nlsLM(RWC_Intercept$Intercept ~ a + b * exp(c * RWC_Intercept$RWC.y),
               data = RWC_Intercept, start = initial_guess,
               control = nls.lm.control(maxiter = 100))
  
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_original <- df_original %>%
    mutate(A_intcpt = ifelse(RWC.y == rwc, a, A_intcpt)) %>%
    mutate(B_intcpt = ifelse(RWC.y == rwc, b, B_intcpt)) %>%
    mutate(C_intcpt = ifelse(RWC.y == rwc, c, C_intcpt))
  
}

##############
## Estimate Slope and Intercept
##############

a <- df_original$A_slope
b <- df_original$B_slope
c <- df_original$C_slope
df_original$Slope_est <- a + b *exp(c *df_original$RWC.y)

a <- df_original$A_intcpt
b <- df_original$B_intcpt
c <- df_original$C_intcpt
df_original$Intercept_est <- a + b *exp(c *df_original$RWC.y)

##############
## Estimate Fr
##############

df_original$Fr_est <- df_original$Intercept_est + 
  df_original$Slope_est * df_original$CAI

# Load the dplyr package
library(dplyr)

# Assuming your data frame is called my_data
df_original <- df_original %>%
  mutate(
    rwc_range = case_when(
      RWC.y < 0.25  ~ "RWC < 25%",
      RWC.y >= 0.25 & RWC.y <= 0.75 ~ "25% < RWC < 75%",
      RWC.y > 0.75  ~ "RWC > 75%",
      TRUE ~ "Other" # This will catch any NA or other unexpected values
    )
  )


# Create the scatter plot
ggplot(df_original, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6, color = "black") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
  labs(x = "Fraction residue cover", y ="Estimated fraction residue cover") +
  theme(legend.title = element_blank())+
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1)) + # Change as needed
  scale_y_continuous(limits = c(0, 1)) + # Change as needed
  coord_fixed(ratio = 1)

lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_original)

# Add a new column to df_original to store the predicted values
df_original$Predictions <- NA  # Initialize with NA

# Populate this column with the predicted values
df_original$Predictions[!is.na(df_original$Fr_est)] <- predict(lm_model, newdata = df_original[!is.na(df_original$Fr_est),])

# Calculate residuals and RMSE only for the rows for which predictions were actually made
valid_rows <- !is.na(df_original$Predictions) & !is.na(df_original$Fraction_Residue_Cover)

# Calculate residuals
residuals <- df_original$Fraction_Residue_Cover[valid_rows] - df_original$Predictions[valid_rows]

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))


##############
# ## Just RWC < %25
# 
# df_original <- dplyr::filter(df_original, rwc_range == 'RWC < 25%')
# 
# # Create the scatter plot
# ggplot(df_original, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
#   geom_point(aes(color = rwc_range), size = 0.6, color = "black") +
#   geom_smooth(method = 'lm', se = FALSE) +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
#   labs(x = "Fraction residue cover", y ="Estimated fraction residue cover") +
#   theme(legend.title = element_blank())+
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 1)) + # Change as needed
#   scale_y_continuous(limits = c(0, 1)) + # Change as needed
#   coord_fixed(ratio = 1)
# 
# lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_original)
# 
# # Add a new column to df_original to store the predicted values
# df_original$Predictions <- NA  # Initialize with NA
# 
# # Populate this column with the predicted values
# df_original$Predictions[!is.na(df_original$Fr_est)] <- predict(lm_model, newdata = df_original[!is.na(df_original$Fr_est),])
# 
# # Calculate residuals and RMSE only for the rows for which predictions were actually made
# valid_rows <- !is.na(df_original$Predictions) & !is.na(df_original$Fraction_Residue_Cover)
# 
# # Calculate residuals
# residuals <- df_original$Fraction_Residue_Cover[valid_rows] - df_original$Predictions[valid_rows]
# 
# # Calculate RMSE
# rmse <- sqrt(mean(residuals^2))
# print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))

df_original$INDEX <- 'SINDRI'
df_original$Method <- 'After EPO'
df_original_SINDRI <- df_original

