
# Load the reshape2 package
library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(minpack.lm)
library(tidyr)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/',
                       'multivariate_model/')

path_to_plot <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/',
                       'multivariate_model/plots/')

Mixed_indices_df <- read.csv(paste0(path_to_data, 
                                  "Mixed_indices_df.csv"),
                           header = TRUE, row.names = NULL)

Mixed_indices_df <- Mixed_indices_df %>%
  mutate(rwc_range = case_when(
    RWC >= 0 & RWC < 0.2 ~ 0.1,
    RWC >= 0.2 & RWC < 0.4 ~ 0.3,
    RWC >= 0.4 & RWC < 0.6 ~ 0.5,
    RWC >= 0.6 & RWC < 0.8 ~ 0.7,
    RWC >= 0.8 & RWC <= 1 ~ 0.9,
    TRUE ~ NA_real_ # for values outside the specified range, if any
  ))
# Mixed_indices_df <- Mixed_indices_df %>%
#   mutate(RWC = as.character(RWC)) %>%
#   separate(col = RWC, into = c("crop_rwc", "soil_rwc"), sep = "_", remove = FALSE)

# Mixed_indices_df$soil_rwc <- as.numeric(Mixed_indices_df$soil_rwc)
# Mixed_indices_df$crop_rwc <- as.numeric(Mixed_indices_df$crop_rwc)

# Mixed_indices_df$mixed_RWC <- Mixed_indices_df$soil_rwc * (1 - Mixed_indices_df$Fr) +
#   Mixed_indices_df$crop_rwc * Mixed_indices_df$Fr
################################################################
################################################################

##################          NDTI        ########################

################################################################
################################################################


# # Perform linear regression (Fr ~ index)
# original_driest_df <- data.frame()
# for (crp in unique(df_mix$crop)){
#   crp_df <- df_mix %>% dplyr:: filter(crop == crp)
#   driest <-   crp_df %>%  dplyr:: filter(RWC.x == min(crp_df$RWC.x))
#   original_driest_df <- rbind(original_driest_df, driest)
# }

mix = unique(Mixed_indices_df$Mix)[1]
df_mix <- dplyr::filter(Mixed_indices_df, Mix == mix)

rwc <- unique(df_mix$rwc_range)[1]
mix_df_slopIntadded <- data.frame()
for (rwc in unique(df_mix$rwc_range)){
  print(rwc)
  df <- dplyr::filter(df_mix, rwc_range == rwc)
  lm_fit <- lm(df$Fr ~ df$NDTI,
               data = df)
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  
  df$slope <- slope
  df$Intercept <- intercept
  mix_df_slopIntadded <- rbind(mix_df_slopIntadded, df)
}  

# mix_df_slopIntadded$a_slope = NA
# mix_df_slopIntadded$b_slope = NA
# mix_df_slopIntadded$c_slope = NA
# mix_df_slopIntadded$d_slope = NA

rwc <- unique(mix_df_slopIntadded$rwc_range)[1]

####
# Fit exponential curve to Slope~RWC

###########
# Slope
###########
# Initial parameter guesses
# initial_guess <- c(a = 1, b = 1, c = 1, d = 1)

# Nonlinear least squares fit
RWC_Slope <- unique(mix_df_slopIntadded[, c("rwc_range", "slope")])

plot(RWC_Slope$rwc_range, RWC_Slope$slope, main = "Scatter Plot of rwc_range vs slope",
     xlab = "rwc_range", ylab = "slope", pch = 19)

# fit <- nlsLM(slope ~ a + b * exp(-0.5 * ((rwc_range-c)/d)^2),
#              data = RWC_Slope, start = initial_guess)
# Quadratic polynomial model
quadratic_model <- lm(slope ~ rwc_range + I(rwc_range^2), data = RWC_Slope)
summary(quadratic_model)

fitted_values <- predict(quadratic_model, newdata = RWC_Slope)

# If you also want to store the coefficients directly:
# Extract coefficients
coefficients <- coef(quadratic_model)


# Extracting coefficients
Slope_intercept <- coefficients[1]
Slope_linear_term <- coefficients[2]
Slope_quadratic_term <- coefficients[3]

df_mix <- df_mix %>%
  mutate(Slope_intercept = ifelse(rwc_range == rwc, Slope_intercept, Slope_intercept)) %>%
  mutate(Slope_linear_term = ifelse(rwc_range == rwc, Slope_linear_term, Slope_linear_term)) %>%
  mutate(Slope_quadratic_term = ifelse(rwc_range == rwc, Slope_quadratic_term, Slope_quadratic_term)) 




#   ###########
#   # Intercept
#   ###########
# Nonlinear least squares fit
RWC_Intercept <- unique(mix_df_slopIntadded[, c("rwc_range", "Intercept")])

plot(RWC_Intercept$rwc_range, RWC_Intercept$Intercept, main = "Scatter Plot of rwc_range vs slope",
     xlab = "rwc_range", ylab = "Intercept", pch = 19)

linear_model <- lm(Intercept ~ rwc_range, data = RWC_Intercept)
summary(linear_model)


fitted_values <- predict(linear_model, newdata = RWC_Intercept)

# If you also want to store the coefficients directly:
# Extract coefficients
coefficients <- coef(linear_model)


# Extracting coefficients
Intercept_intercept <- coefficients[1]
Intercept_slope<- coefficients[2]


df_mix <- df_mix %>%
  mutate(Intercept_intercept = ifelse(rwc_range == rwc, Intercept_intercept, Intercept_intercept)) %>%
  mutate(Intercept_slope = ifelse(rwc_range == rwc, Intercept_slope, Intercept_slope)) 

##############
## Compare models R2 for the same crop
##############



##############
## Compare models R2 for other crops
##############

mix = unique(Mixed_indices_df$Mix)[13]
df_mix_new <- dplyr::filter(Mixed_indices_df, Mix == mix)

# Calculate slop and intercept for all rwc combined (baseline)
lm_fit <- lm(df_mix_new$Fr ~ df_mix_new$NDTI,
             data = df_mix_new)
slope <- coef(lm_fit)[2]
intercept <- coef(lm_fit)[1]

df_mix_new$Slope_baseline <- slope
df_mix_new$Intercept_baseline <- intercept
df_mix_new$Fr_baseline <- df_mix_new$Intercept_baseline +
  df_mix_new$Slope_baseline * df_mix_new$NDTI

pearson_correlation <- cor(df_mix_new$Fr, df_mix_new$Fr_baseline,
                           method = "pearson")

df_mix_new$pearson_correlation_baseline <- pearson_correlation

df_mix_new$Slope_intercept <- df_mix$Slope_intercept
df_mix_new$Slope_linear_term <- df_mix$Slope_linear_term
df_mix_new$Slope_quadratic_term <- df_mix$Slope_quadratic_term
df_mix_new$Intercept_intercept <- df_mix$Intercept_intercept
df_mix_new$Intercept_slope <- df_mix$Intercept_slope

# Estimate new slope and intercept
df_mix_new$new_Slope <- df_mix_new$Slope_intercept +
  (df_mix_new$Slope_linear_term * df_mix_new$rwc_range) +
  (df_mix_new$Slope_quadratic_term * df_mix_new$rwc_range^2)
df_mix_new$new_Intercept <- df_mix_new$Intercept_intercept +
  df_mix_new$Intercept_slope * df_mix_new$rwc_range

df_mix_new$Fr_newModel <- df_mix_new$new_Intercept + df_mix_new$NDTI *
  df_mix_new$Intercept_slope

pearson_correlation_new <- cor(df_mix_new$Fr, df_mix_new$Fr_newModel,
                           method = "pearson")

print(pearson_correlation)
print(pearson_correlation_new)






