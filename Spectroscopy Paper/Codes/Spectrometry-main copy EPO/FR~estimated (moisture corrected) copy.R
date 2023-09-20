
# Load the reshape2 package
library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(minpack.lm)


# Set the path to directory
path_to_directory <- "/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl"

# Get a list of all .csv files in the directory
all_csv_files <- list.files(path = path_to_directory, pattern = "\\.csv$", full.names = FALSE)

# Remove the ".csv" extension from the file names
csv_file_fullname <- sub("\\.csv$", "", all_csv_files)

# Extract characters after the second underscore
crop_soil_names <- sapply(strsplit(csv_file_fullname, "_"), function(x) paste(tail(x, -2), collapse = "_"))
print(unique(crop_soil_names))

crops <- unique(sapply(strsplit(crop_soil_names, "_"), function(x) paste(x[1])))
soils <- unique(sapply(strsplit(crop_soil_names, "_"), function(x) paste(x[2])))

setwd("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO")

CROP = "Canola"

################################################################
################################################################

##################          NDTI        ########################

################################################################
################################################################

df_original <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0('/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl/NDTI_original_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_original <- rbind(df_original, index_df)
  
}

df_original$Slope = NA
df_original$Intercept = NA


# Perform linear regression (Fr ~ index)


original_driest_df <- data.frame()
for (crp in unique(df_original$crop)){
  crp_df <- df_original %>% dplyr:: filter(crop == crp)
  driest <-   crp_df %>%  dplyr:: filter(RWC.x == min(crp_df$RWC.x))
  original_driest_df <- rbind(original_driest_df, driest)
}

for (rwc in unique(df_original$RWC.x)){
  print(rwc)
  df <- dplyr::filter(df_original, RWC.x == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  
  slope_list <- append(slope_list, slope)
  intcp_list <- append(intcp_list, intercept)
  
  df_original <- df_original %>%
    mutate(Slope = ifelse(RWC.x == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.x == rwc, intercept, Intercept))
}  

df_original$A_slope = NA
df_original$B_slope = NA
df_original$C_slope = NA

for (rwc in unique(df_original$RWC.x)){
  ####
  # Fit exponential curve to Slope~RWC
  
  ###########
  # Slope
  ###########
  # Initial parameter guesses
  initial_guess <- c(a = 1, b = 1, c = 1)

  # Nonlinear least squares fit
  RWC_Slope <- unique(df_original[, c("RWC.x", "Slope")])

  fit <- nlsLM(RWC_Slope$Slope ~ a + b * exp(c * RWC_Slope$RWC.x),
               data = RWC_Slope, start = initial_guess)
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]

  df_original <- df_original %>%
    mutate(A_slope = ifelse(RWC.x == rwc, a, A_slope)) %>%
    mutate(B_slope = ifelse(RWC.x == rwc, b, B_slope)) %>%
    mutate(C_slope = ifelse(RWC.x == rwc, c, C_slope))
}



#   ###########
#   # Intercept
#   ###########

df_original$A_intcpt = NA
df_original$B_intcpt = NA
df_original$C_intcpt = NA

for (rwc in unique(df_original$RWC.x)){

  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Intercept <- unique(df_original[, c("RWC.x", "Intercept")])
  
  fit <- nlsLM(RWC_Intercept$Intercept ~ a + b * exp(c * RWC_Intercept$RWC.x),
               data = RWC_Intercept, start = initial_guess,
                control = nls.lm.control(maxiter = 100))
  
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_original <- df_original %>%
    mutate(A_intcpt = ifelse(RWC.x == rwc, a, A_intcpt)) %>%
    mutate(B_intcpt = ifelse(RWC.x == rwc, b, B_intcpt)) %>%
    mutate(C_intcpt = ifelse(RWC.x == rwc, c, C_intcpt))
  
}

##############
## Estimate Slope and Intercept
##############

a <- df_original$A_slope
b <- df_original$B_slope
c <- df_original$C_slope
df_original$Slope_est <- a + b *exp(c *df_original$RWC.x)

a <- df_original$A_intcpt
b <- df_original$B_intcpt
c <- df_original$C_intcpt
df_original$Intercept_est <- a + b *exp(c *df_original$RWC.x)

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
      RWC.x < 0.25  ~ "RWC < 25%",
      RWC.x >= 0.25 & RWC.x <= 0.75 ~ "25% < RWC < 75%",
      RWC.x > 0.75  ~ "RWC > 75%",
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


# ##############
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

df_original$INDEX <- 'NDTI'
df_original$Method <- 'Before EPO'
df_original_NDTI <- df_original

################################################################
################################################################

#####################        CAI        ######################

################################################################
################################################################

df_original <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0('/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl/CAI_original_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_original <- rbind(df_original, index_df)
  
}

df_original$Slope = NA
df_original$Intercept = NA


# Perform linear regression (Fr ~ index)
for (rwc in unique(df_original$RWC.x)){
  df <- dplyr::filter(df_original, RWC.x == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_original <- df_original %>%
    mutate(Slope = ifelse(RWC.x == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.x == rwc, intercept, Intercept))
}  

df_original$A_slope = NA
df_original$B_slope = NA
df_original$C_slope = NA

for (rwc in unique(df_original$RWC.x)){
  ####
  # Fit exponential curve to Slope~RWC
  
  ###########
  # Slope
  ###########
  # Initial parameter guesses
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Slope <- unique(df_original[, c("RWC.x", "Slope")])
  
  fit <- nlsLM(RWC_Slope$Slope ~ a + b * exp(c * RWC_Slope$RWC.x),
               data = RWC_Slope, start = initial_guess)
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_original <- df_original %>%
    mutate(A_slope = ifelse(RWC.x == rwc, a, A_slope)) %>%
    mutate(B_slope = ifelse(RWC.x == rwc, b, B_slope)) %>%
    mutate(C_slope = ifelse(RWC.x == rwc, c, C_slope))
}



#   ###########
#   # Intercept
#   ###########

df_original$A_intcpt = NA
df_original$B_intcpt = NA
df_original$C_intcpt = NA

for (rwc in unique(df_original$RWC.x)){
  
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Intercept <- unique(df_original[, c("RWC.x", "Intercept")])
  
  fit <- nlsLM(RWC_Intercept$Intercept ~ a + b * exp(c * RWC_Intercept$RWC.x),
               data = RWC_Intercept, start = initial_guess,
               control = nls.lm.control(maxiter = 100))
  
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_original <- df_original %>%
    mutate(A_intcpt = ifelse(RWC.x == rwc, a, A_intcpt)) %>%
    mutate(B_intcpt = ifelse(RWC.x == rwc, b, B_intcpt)) %>%
    mutate(C_intcpt = ifelse(RWC.x == rwc, c, C_intcpt))
  
}

##############
## Estimate Slope and Intercept
##############

a <- df_original$A_slope
b <- df_original$B_slope
c <- df_original$C_slope
df_original$Slope_est <- a + b *exp(c *df_original$RWC.x)

a <- df_original$A_intcpt
b <- df_original$B_intcpt
c <- df_original$C_intcpt
df_original$Intercept_est <- a + b *exp(c *df_original$RWC.x)

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
      RWC.x < 0.25  ~ "RWC < 25%",
      RWC.x >= 0.25 & RWC.x <= 0.75 ~ "25% < RWC < 75%",
      RWC.x > 0.75  ~ "RWC > 75%",
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


# ##############
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

df_original$INDEX <- 'CAI'
df_original$Method <- 'Before EPO'
df_original_CAI <- df_original

################################################################
################################################################

##################          SINDRI        ########################

################################################################
################################################################

df_original <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0('/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl/SINDRI_original_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_original <- rbind(df_original, index_df)
  
}

df_original$Slope = NA
df_original$Intercept = NA


# Perform linear regression (Fr ~ index)
for (rwc in unique(df_original$RWC.x)){
  df <- dplyr::filter(df_original, RWC.x == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_original <- df_original %>%
    mutate(Slope = ifelse(RWC.x == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.x == rwc, intercept, Intercept))
}  

df_original$A_slope = NA
df_original$B_slope = NA
df_original$C_slope = NA

for (rwc in unique(df_original$RWC.x)){
  ####
  # Fit exponential curve to Slope~RWC
  
  ###########
  # Slope
  ###########
  # Initial parameter guesses
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Slope <- unique(df_original[, c("RWC.x", "Slope")])
  
  fit <- nlsLM(RWC_Slope$Slope ~ a + b * exp(c * RWC_Slope$RWC.x),
               data = RWC_Slope, start = initial_guess)
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_original <- df_original %>%
    mutate(A_slope = ifelse(RWC.x == rwc, a, A_slope)) %>%
    mutate(B_slope = ifelse(RWC.x == rwc, b, B_slope)) %>%
    mutate(C_slope = ifelse(RWC.x == rwc, c, C_slope))
}



#   ###########
#   # Intercept
#   ###########

df_original$A_intcpt = NA
df_original$B_intcpt = NA
df_original$C_intcpt = NA

for (rwc in unique(df_original$RWC.x)){
  
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Intercept <- unique(df_original[, c("RWC.x", "Intercept")])
  
  fit <- nlsLM(RWC_Intercept$Intercept ~ a + b * exp(c * RWC_Intercept$RWC.x),
               data = RWC_Intercept, start = initial_guess,
               control = nls.lm.control(maxiter = 100))
  
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_original <- df_original %>%
    mutate(A_intcpt = ifelse(RWC.x == rwc, a, A_intcpt)) %>%
    mutate(B_intcpt = ifelse(RWC.x == rwc, b, B_intcpt)) %>%
    mutate(C_intcpt = ifelse(RWC.x == rwc, c, C_intcpt))
  
}

##############
## Estimate Slope and Intercept
##############

a <- df_original$A_slope
b <- df_original$B_slope
c <- df_original$C_slope
df_original$Slope_est <- a + b *exp(c *df_original$RWC.x)

a <- df_original$A_intcpt
b <- df_original$B_intcpt
c <- df_original$C_intcpt
df_original$Intercept_est <- a + b *exp(c *df_original$RWC.x)

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
      RWC.x < 0.25  ~ "RWC < 25%",
      RWC.x >= 0.25 & RWC.x <= 0.75 ~ "25% < RWC < 75%",
      RWC.x > 0.75  ~ "RWC > 75%",
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


# ##############
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
df_original$Method <- 'Before EPO'
df_original_SINDRI <- df_original