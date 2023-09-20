

#############################################################################
#############################################################################
#############################################################################


                                # BEFORE EPO


#############################################################################
#############################################################################
#############################################################################

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
##################          NDTI        ########################
################################################################

df_original <- data.frame()
for (sl in soils){
  for (crp in crops){
  index_df <- read.csv(paste0('/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl/NDTI_original_', crp, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- crp
  df_original <- rbind(df_original, index_df)
  
  }
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

df_original$INDEX <- 'NDTI'
df_original$Method <- 'Before EPO'
df_original$RMSE <- round(rmse, 4)
df_original_NDTI <- df_original

################################################################
#####################        CAI        ######################
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
df_original$RMSE <- round(rmse, 4)
df_original_CAI <- df_original

################################################################
##################          SINDRI        ######################
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
df_original$RMSE <- round(rmse, 4)
df_original_SINDRI <- df_original



#############################################################################
#############################################################################
#############################################################################


# AFTER EPO


#############################################################################
#############################################################################
#############################################################################


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

CROP = "Peas"

################################################################
##################          NDTI            ####################
################################################################

df_transformed <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0('/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl/NDTI_transformed_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_transformed <- rbind(df_transformed, index_df)
  
}

df_transformed$Slope = NA
df_transformed$Intercept = NA


# Perform linear regression (Fr ~ index)
for (rwc in unique(df_transformed$RWC.x)){
  df <- dplyr::filter(df_transformed, RWC.x == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_transformed <- df_transformed %>%
    mutate(Slope = ifelse(RWC.x == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.x == rwc, intercept, Intercept))
}  

df_transformed$A_slope = NA
df_transformed$B_slope = NA
df_transformed$C_slope = NA

for (rwc in unique(df_transformed$RWC.x)){
  ####
  # Fit exponential curve to Slope~RWC
  
  ###########
  # Slope
  ###########
  # Initial parameter guesses
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Slope <- unique(df_transformed[, c("RWC.x", "Slope")])
  
  fit <- nlsLM(RWC_Slope$Slope ~ a + b * exp(c * RWC_Slope$RWC.x),
               data = RWC_Slope, start = initial_guess)
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_transformed <- df_transformed %>%
    mutate(A_slope = ifelse(RWC.x == rwc, a, A_slope)) %>%
    mutate(B_slope = ifelse(RWC.x == rwc, b, B_slope)) %>%
    mutate(C_slope = ifelse(RWC.x == rwc, c, C_slope))
}



#   ###########
#   # Intercept
#   ###########

df_transformed$A_intcpt = NA
df_transformed$B_intcpt = NA
df_transformed$C_intcpt = NA

for (rwc in unique(df_transformed$RWC.x)){
  
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Intercept <- unique(df_transformed[, c("RWC.x", "Intercept")])
  
  fit <- nlsLM(RWC_Intercept$Intercept ~ a + b * exp(c * RWC_Intercept$RWC.x),
               data = RWC_Intercept, start = initial_guess,
               control = nls.lm.control(maxiter = 100))
  
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_transformed <- df_transformed %>%
    mutate(A_intcpt = ifelse(RWC.x == rwc, a, A_intcpt)) %>%
    mutate(B_intcpt = ifelse(RWC.x == rwc, b, B_intcpt)) %>%
    mutate(C_intcpt = ifelse(RWC.x == rwc, c, C_intcpt))
  
}

##############
## Estimate Slope and Intercept
##############

a <- df_transformed$A_slope
b <- df_transformed$B_slope
c <- df_transformed$C_slope
df_transformed$Slope_est <- a + b *exp(c *df_transformed$RWC.x)

a <- df_transformed$A_intcpt
b <- df_transformed$B_intcpt
c <- df_transformed$C_intcpt
df_transformed$Intercept_est <- a + b *exp(c *df_transformed$RWC.x)

##############
## Estimate Fr
##############

df_transformed$Fr_est <- df_transformed$Intercept_est + 
  df_transformed$Slope_est * df_transformed$CAI

# Load the dplyr package
library(dplyr)

# Assuming your data frame is called my_data
df_transformed <- df_transformed %>%
  mutate(
    rwc_range = case_when(
      RWC.x < 0.25  ~ "RWC < 25%",
      RWC.x >= 0.25 & RWC.x <= 0.75 ~ "25% < RWC < 75%",
      RWC.x > 0.75  ~ "RWC > 75%",
      TRUE ~ "Other" # This will catch any NA or other unexpected values
    )
  )


# Create the scatter plot
ggplot(df_transformed, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6, color = "black") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
  labs(x = "Fraction residue cover", y ="Estimated fraction residue cover") +
  theme(legend.title = element_blank())+
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1)) + # Change as needed
  scale_y_continuous(limits = c(0, 1)) + # Change as needed
  coord_fixed(ratio = 1)

lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_transformed)

# Add a new column to df_transformed to store the predicted values
df_transformed$Predictions <- NA  # Initialize with NA

# Populate this column with the predicted values
df_transformed$Predictions[!is.na(df_transformed$Fr_est)] <- predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est),])

# Calculate residuals and RMSE only for the rows for which predictions were actually made
valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)

# Calculate residuals
residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))


# ##############
# ## Just RWC < %25
# 
# df_transformed <- dplyr::filter(df_transformed, rwc_range == 'RWC < 25%')
# 
# # Create the scatter plot
# ggplot(df_transformed, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
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
# lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_transformed)
# 
# # Add a new column to df_transformed to store the predicted values
# df_transformed$Predictions <- NA  # Initialize with NA
# 
# # Populate this column with the predicted values
# df_transformed$Predictions[!is.na(df_transformed$Fr_est)] <- predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est),])
# 
# # Calculate residuals and RMSE only for the rows for which predictions were actually made
# valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)
# 
# # Calculate residuals
# residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]
# 
# # Calculate RMSE
# rmse <- sqrt(mean(residuals^2))
# print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))

df_transformed$INDEX <- 'NDTI'
df_transformed$Method <- 'After EPO'
df_transformed$RMSE <- round(rmse, 4)
df_transformed_NDTI <- df_transformed

################################################################
#####################        CAI        ######################
################################################################

df_transformed <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0('/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl/CAI_transformed_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_transformed <- rbind(df_transformed, index_df)
  
}

df_transformed$Slope = NA
df_transformed$Intercept = NA


# Perform linear regression (Fr ~ index)
for (rwc in unique(df_transformed$RWC.x)){
  df <- dplyr::filter(df_transformed, RWC.x == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_transformed <- df_transformed %>%
    mutate(Slope = ifelse(RWC.x == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.x == rwc, intercept, Intercept))
}  

df_transformed$A_slope = NA
df_transformed$B_slope = NA
df_transformed$C_slope = NA

for (rwc in unique(df_transformed$RWC.x)){
  ####
  # Fit exponential curve to Slope~RWC
  
  ###########
  # Slope
  ###########
  # Initial parameter guesses
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Slope <- unique(df_transformed[, c("RWC.x", "Slope")])
  
  fit <- nlsLM(RWC_Slope$Slope ~ a + b * exp(c * RWC_Slope$RWC.x),
               data = RWC_Slope, start = initial_guess)
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_transformed <- df_transformed %>%
    mutate(A_slope = ifelse(RWC.x == rwc, a, A_slope)) %>%
    mutate(B_slope = ifelse(RWC.x == rwc, b, B_slope)) %>%
    mutate(C_slope = ifelse(RWC.x == rwc, c, C_slope))
}



#   ###########
#   # Intercept
#   ###########

df_transformed$A_intcpt = NA
df_transformed$B_intcpt = NA
df_transformed$C_intcpt = NA

for (rwc in unique(df_transformed$RWC.x)){
  
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Intercept <- unique(df_transformed[, c("RWC.x", "Intercept")])
  
  fit <- nlsLM(RWC_Intercept$Intercept ~ a + b * exp(c * RWC_Intercept$RWC.x),
               data = RWC_Intercept, start = initial_guess,
               control = nls.lm.control(maxiter = 100))
  
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_transformed <- df_transformed %>%
    mutate(A_intcpt = ifelse(RWC.x == rwc, a, A_intcpt)) %>%
    mutate(B_intcpt = ifelse(RWC.x == rwc, b, B_intcpt)) %>%
    mutate(C_intcpt = ifelse(RWC.x == rwc, c, C_intcpt))
  
}

##############
## Estimate Slope and Intercept
##############

a <- df_transformed$A_slope
b <- df_transformed$B_slope
c <- df_transformed$C_slope
df_transformed$Slope_est <- a + b *exp(c *df_transformed$RWC.x)

a <- df_transformed$A_intcpt
b <- df_transformed$B_intcpt
c <- df_transformed$C_intcpt
df_transformed$Intercept_est <- a + b *exp(c *df_transformed$RWC.x)

##############
## Estimate Fr
##############

df_transformed$Fr_est <- df_transformed$Intercept_est + 
  df_transformed$Slope_est * df_transformed$CAI

# Load the dplyr package
library(dplyr)

# Assuming your data frame is called my_data
df_transformed <- df_transformed %>%
  mutate(
    rwc_range = case_when(
      RWC.x < 0.25  ~ "RWC < 25%",
      RWC.x >= 0.25 & RWC.x <= 0.75 ~ "25% < RWC < 75%",
      RWC.x > 0.75  ~ "RWC > 75%",
      TRUE ~ "Other" # This will catch any NA or other unexpected values
    )
  )


# Create the scatter plot
ggplot(df_transformed, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6, color = "black") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
  labs(x = "Fraction residue cover", y ="Estimated fraction residue cover") +
  theme(legend.title = element_blank())+
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1)) + # Change as needed
  scale_y_continuous(limits = c(0, 1)) + # Change as needed
  coord_fixed(ratio = 1)

lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_transformed)

# Add a new column to df_transformed to store the predicted values
df_transformed$Predictions <- NA  # Initialize with NA

# Populate this column with the predicted values
df_transformed$Predictions[!is.na(df_transformed$Fr_est)] <- predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est),])

# Calculate residuals and RMSE only for the rows for which predictions were actually made
valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)

# Calculate residuals
residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))

# 
# ##############
# ## Just RWC < %25
# 
# df_transformed <- dplyr::filter(df_transformed, rwc_range == 'RWC < 25%')
# 
# # Create the scatter plot
# ggplot(df_transformed, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
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
# lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_transformed)
# 
# # Add a new column to df_transformed to store the predicted values
# df_transformed$Predictions <- NA  # Initialize with NA
# 
# # Populate this column with the predicted values
# df_transformed$Predictions[!is.na(df_transformed$Fr_est)] <- predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est),])
# 
# # Calculate residuals and RMSE only for the rows for which predictions were actually made
# valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)
# 
# # Calculate residuals
# residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]
# 
# # Calculate RMSE
# rmse <- sqrt(mean(residuals^2))
# print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))


df_transformed$INDEX <- 'CAI'
df_transformed$Method <- 'After EPO'
df_transformed$RMSE <- round(rmse, 4)
df_transformed_CAI <- df_transformed


################################################################
##################          SINDRI        ######################
################################################################

df_transformed <- data.frame()
for (sl in soils){
  index_df <- read.csv(paste0('/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main copy EPO/index_org_trsfed_crp_sl/SINDRI_transformed_', CROP, '_', paste0(sl), '.csv'))
  index_df$soil <- sl
  index_df$crop <- CROP
  df_transformed <- rbind(df_transformed, index_df)
  
}

df_transformed$Slope = NA
df_transformed$Intercept = NA


# Perform linear regression (Fr ~ index)
for (rwc in unique(df_transformed$RWC.x)){
  df <- dplyr::filter(df_transformed, RWC.x == rwc)
  lm_fit <- lm(df$Fraction_Residue_Cover ~ df$CAI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  df_transformed <- df_transformed %>%
    mutate(Slope = ifelse(RWC.x == rwc, slope,Slope)) %>% 
    mutate(Intercept = ifelse(RWC.x == rwc, intercept, Intercept))
}  

df_transformed$A_slope = NA
df_transformed$B_slope = NA
df_transformed$C_slope = NA

for (rwc in unique(df_transformed$RWC.x)){
  ####
  # Fit exponential curve to Slope~RWC
  
  ###########
  # Slope
  ###########
  # Initial parameter guesses
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Slope <- unique(df_transformed[, c("RWC.x", "Slope")])
  
  fit <- nlsLM(RWC_Slope$Slope ~ a + b * exp(c * RWC_Slope$RWC.x),
               data = RWC_Slope, start = initial_guess)
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_transformed <- df_transformed %>%
    mutate(A_slope = ifelse(RWC.x == rwc, a, A_slope)) %>%
    mutate(B_slope = ifelse(RWC.x == rwc, b, B_slope)) %>%
    mutate(C_slope = ifelse(RWC.x == rwc, c, C_slope))
}



#   ###########
#   # Intercept
#   ###########

df_transformed$A_intcpt = NA
df_transformed$B_intcpt = NA
df_transformed$C_intcpt = NA

for (rwc in unique(df_transformed$RWC.x)){
  
  initial_guess <- c(a = 1, b = 1, c = 1)
  
  # Nonlinear least squares fit
  RWC_Intercept <- unique(df_transformed[, c("RWC.x", "Intercept")])
  
  fit <- nlsLM(RWC_Intercept$Intercept ~ a + b * exp(c * RWC_Intercept$RWC.x),
               data = RWC_Intercept, start = initial_guess,
               control = nls.lm.control(maxiter = 100))
  
  # Extracting coefficients
  a <- coef(fit)["a"]
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  
  df_transformed <- df_transformed %>%
    mutate(A_intcpt = ifelse(RWC.x == rwc, a, A_intcpt)) %>%
    mutate(B_intcpt = ifelse(RWC.x == rwc, b, B_intcpt)) %>%
    mutate(C_intcpt = ifelse(RWC.x == rwc, c, C_intcpt))
  
}

##############
## Estimate Slope and Intercept
##############

a <- df_transformed$A_slope
b <- df_transformed$B_slope
c <- df_transformed$C_slope
df_transformed$Slope_est <- a + b *exp(c *df_transformed$RWC.x)

a <- df_transformed$A_intcpt
b <- df_transformed$B_intcpt
c <- df_transformed$C_intcpt
df_transformed$Intercept_est <- a + b *exp(c *df_transformed$RWC.x)

##############
## Estimate Fr
##############

df_transformed$Fr_est <- df_transformed$Intercept_est + 
  df_transformed$Slope_est * df_transformed$CAI

# Load the dplyr package
library(dplyr)

# Assuming your data frame is called my_data
df_transformed <- df_transformed %>%
  mutate(
    rwc_range = case_when(
      RWC.x < 0.25  ~ "RWC < 25%",
      RWC.x >= 0.25 & RWC.x <= 0.75 ~ "25% < RWC < 75%",
      RWC.x > 0.75  ~ "RWC > 75%",
      TRUE ~ "Other" # This will catch any NA or other unexpected values
    )
  )


# Create the scatter plot
ggplot(df_transformed, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6, color = "black") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
  labs(x = "Fraction residue cover", y ="Estimated fraction residue cover") +
  theme(legend.title = element_blank())+
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1)) + # Change as needed
  scale_y_continuous(limits = c(0, 1)) + # Change as needed
  coord_fixed(ratio = 1)

lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_transformed)

# Add a new column to df_transformed to store the predicted values
df_transformed$Predictions <- NA  # Initialize with NA

# Populate this column with the predicted values
df_transformed$Predictions[!is.na(df_transformed$Fr_est)] <- predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est),])

# Calculate residuals and RMSE only for the rows for which predictions were actually made
valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)

# Calculate residuals
residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))


##############
# ## Just RWC < %25
# 
# df_transformed <- dplyr::filter(df_transformed, rwc_range == 'RWC < 25%')
# 
# # Create the scatter plot
# ggplot(df_transformed, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
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
# lm_model <- lm(Fraction_Residue_Cover ~ Fr_est, data = df_transformed)
# 
# # Add a new column to df_transformed to store the predicted values
# df_transformed$Predictions <- NA  # Initialize with NA
# 
# # Populate this column with the predicted values
# df_transformed$Predictions[!is.na(df_transformed$Fr_est)] <- predict(lm_model, newdata = df_transformed[!is.na(df_transformed$Fr_est),])
# 
# # Calculate residuals and RMSE only for the rows for which predictions were actually made
# valid_rows <- !is.na(df_transformed$Predictions) & !is.na(df_transformed$Fraction_Residue_Cover)
# 
# # Calculate residuals
# residuals <- df_transformed$Fraction_Residue_Cover[valid_rows] - df_transformed$Predictions[valid_rows]
# 
# # Calculate RMSE
# rmse <- sqrt(mean(residuals^2))
# print(paste("Root Mean Square Error (RMSE):", round(rmse, 4)))

df_transformed$INDEX <- 'SINDRI'
df_transformed$Method <- 'After EPO'
df_transformed$RMSE <- round(rmse, 4)
df_transformed_SINDRI <- df_transformed

df <- rbind(df_original_CAI, df_original_NDTI, df_original_SINDRI,
            df_transformed_CAI, df_transformed_NDTI, df_transformed_SINDRI)

df$Method <- factor(df$Method, levels = c("Before EPO", "After EPO"))

ggplot(df, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6, color = "black") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "red") + 
  labs(x = "Fraction residue cover", y = "Estimated fraction residue cover") +
  theme(legend.title = element_blank(), 
        panel.background = element_rect(fill = "white"),  # White background
        panel.grid = element_blank(),  # Remove grid lines
        axis.line = element_line(colour = "black")  # Add axis lines
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1)) + # Change as needed
  scale_y_continuous(limits = c(0, 1)) + # Change as needed
  coord_fixed(ratio = 1) +
  facet_grid(INDEX ~ Method) # Facet grid based on "INDEX" and "Method"

# Save the figure as a PDF with A5 size (width = 14.8 cm, height = 21 cm)
ggsave(paste0("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/plots/act Fr~est FR/act Fr~est_Fr (all crops and soils).png"), width = 21, height = 14.8, units = "cm")

df$rwc_range <- as.factor(df$rwc_range)
df$rwc_range <- factor(df$rwc_range, levels = c("RWC < 25%", "25% < RWC < 75%", "RWC > 75%"))

ggplot(df, aes(y = Fr_est, x = Fraction_Residue_Cover)) +
  geom_point(aes(color = rwc_range), size = 0.6, alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1, color = "black") + 
  labs(x = "Fraction residue cover", y = "Estimated fraction residue cover") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.key.size = unit(1, "lines"),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    panel.grid.major = element_line(color = "grey90"),  # Light grey major grid
    panel.grid.minor = element_line(color = "grey95")  # Light grey minor grid
  ) +
  scale_x_continuous(limits = c(0, 1)) +  # Change as needed
  scale_y_continuous(limits = c(0, 1)) +  # Change as needed
  coord_fixed(ratio = 1) +
  facet_grid(INDEX ~ Method) +  # Facet grid based on "INDEX" and "Method"
  scale_color_manual(values = c("RWC < 25%" = "#EAE30D", "25% < RWC < 75%" = "#0BA04A", "RWC > 75%" = "#3F0BA0"))
# Use viridis palette for 'color'
# Save the figure as a PDF with A5 size (width = 14.8 cm, height = 21 cm)
ggsave(paste0("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/plots/act Fr~est FR/act Fr~est_Fr (all crops and soils) with rwc range.png"), width = 21, height = 14.8, units = "cm")


ggplot(df, aes(x = Fraction_Residue_Cover, y = Fr_est)) +
  geom_point(aes(color = rwc_range)) +
  scale_color_viridis(option = "D")

