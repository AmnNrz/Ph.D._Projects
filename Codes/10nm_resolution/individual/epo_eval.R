library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

Pr <- read.csv(paste0(path_to_data, "Pr_fresh_dark.csv"), check.names = FALSE)
Ps <- read.csv(paste0(path_to_data, "Ps_fresh_dark.csv"), check.names = FALSE)
fitted_df <- read.csv(paste0(path_to_data, "fitted_df_fresh_dark.csv"),
                      check.names = FALSE)
mixed_original <- read.csv(paste0(path_to_data, 'mixed_original.csv'),
                           check.names = FALSE)
mixed_original$Type <- paste0(
  mixed_original$Crop, "_", mixed_original$Soil)

mixed_original <- mixed_original %>%
  mutate(RWC_ave = (
    mixed_original$crop_rwc + mixed_original$soil_rwc) / 2) 
mixed_original$Scan <- mixed_original$Scan

mixed_original <- mixed_original %>% 
  select(-Fraction, everything(), Fraction) %>% 
  select(c("crop_rwc", "soil_rwc", "Scan", "500":ncol(mixed_original))) %>%
  select("Type", "RWC_ave", "Fraction", "Scan", everything())

mixed_original <- mixed_original %>% 
  pivot_longer(cols = '500':names(mixed_original)[ncol(mixed_original)],
               names_to = 'Wvl',
               values_to = 'Reflect') 


fresh_crops <- c("Canola", "Garbanzo Beans", "Peas",
                 "Wheat Norwest Duet", "Wheat Pritchett")

weathered_crops <- c("Weathered Canola",  "Weathered Wheat")

dark_soils <- c("Bagdad", "Mondovi 1", "Athena")

light_soils <- c("Benwy", "Shano", "Lance")

fresh_dark <- expand.grid(fresh_crops, dark_soils)
fresh_dark$mix <- paste(fresh_dark$Var1, fresh_dark$Var2, sep = "_")
fresh_dark <- fresh_dark %>% select(-c(1,2))

fresh_light <- expand.grid(fresh_crops, light_soils)
fresh_light$mix <- paste(fresh_light$Var1, fresh_light$Var2, sep = "_")
fresh_light <- fresh_light %>% select(-c(1,2))

weathered_light <- expand.grid(weathered_crops, light_soils)
weathered_light$mix <- paste(weathered_light$Var1, weathered_light$Var2, sep = "_")
weathered_light <- weathered_light %>% select(-c(1,2))

weathered_dark <- expand.grid(weathered_crops, dark_soils)
weathered_dark$mix <- paste(weathered_dark$Var1, weathered_dark$Var2, sep = "_")
weathered_dark <- weathered_dark %>% select(-c(1,2))

mix_group <- weathered_dark
# mix_group_name <- "fresh_dark"


mix <- unique(mixed_original$Type)[1]
fr <- unique(mixed_original$Fraction)[1]
combined_df <- data.frame()
for (mix in unique(mixed_original$Type)){
  for (fr in unique(mixed_original$Fraction)){
    mixed_original_filtered <- dplyr::filter(mixed_original, Type == mix)
    mixed_original_filtered <- dplyr::filter(mixed_original_filtered, Fraction == fr)
    
    Xsr <- mixed_original_filtered$Reflect
    Xsr <- as.matrix(Xsr)
    # Repeat column 3 times
    Xsr <- Xsr[, rep(1, 19)]
    
    Xsr_hat <- 1/2 * 
      (Xsr %*% as.matrix(Ps) %*% as.matrix(Pr) + 
         Xsr %*% as.matrix(Pr) %*% as.matrix(Ps))
    mixed_original_filtered$Reflect_val <- Xsr_hat[, 1]
    combined_df <- rbind(combined_df, mixed_original_filtered)
    # rownames(Xsr_hat) <- mixed_original_filtered$Wvl
  }
}

combined_df <- combined_df %>% 
  select(-c("RWC_ave", "Scan", "soil_rwc", "Reflect"))

combined_df <- combined_df %>%
  pivot_wider(names_from = Wvl, values_from = Reflect_val)

select_columns_range <- function(df, start_col_name, end_col_name) {
  start_col <- which(names(df) == start_col_name)
  end_col <- which(names(df) == end_col_name)
  if (start_col == 0 || end_col == 0) {
    stop("One of the specified column names does not exist in the dataframe.")
  }
  selected_df <- df[, start_col:end_col]
  return(selected_df)
}

combined_df$CAI <-
  (0.5 * (combined_df$`2030` + combined_df$`2140`) -
     combined_df$`2070`)

combined_df$R2220_2260 <-  
  rowMeans(select_columns_range(combined_df, '2240', '2260'))
combined_df$R2260_2280 <-  
  rowMeans(select_columns_range(combined_df, '2290', '2330'))
combined_df$SINDRI <- 
  (combined_df$R2220_2260 - combined_df$R2260_2280) / 
  (combined_df$R2220_2260 + combined_df$R2260_2280)

combined_df$R1660_1690 <-  
  rowMeans(select_columns_range(combined_df, '1660', '1690'))
combined_df$R2220_2280 <-  
  rowMeans(select_columns_range(combined_df, '2220', '2280'))
combined_df$NDTI <- 
  (combined_df$R1660_1690 - combined_df$R2220_2280) / 
  (combined_df$R1660_1690 + combined_df$R2220_2280)



combined_df_filtered <- combined_df[
  combined_df$Type %in% mix_group$mix, 
]

fitted_df_NDTI <- dplyr::filter(fitted_df, source == "EPO" & Index == "NDTI")

combined_df_filtered$fresh_dark_NDTI_EPO_slope <- fitted_df_NDTI$slope[1]
combined_df_filtered$fresh_dark_NDTI_EPO_intercept <- fitted_df_NDTI$intercept[1]

combined_df_filtered$Fraction_est <- 
  combined_df_filtered$fresh_dark_NDTI_EPO_slope * 
  combined_df_filtered$NDTI+
  combined_df_filtered$fresh_dark_NDTI_EPO_intercept


library(ggplot2)

# Assuming your dataframe is called df and the columns are named x and y
ggplot(combined_df_filtered, aes(x = Fraction_est, y = Fraction)) +
  geom_point() +  # Scatter plot
  geom_abline(intercept = 0, slope = 1, color = "red") +  # 45-degree line
  labs(x = "X", y = "Y", title = "Scatter Plot")  # Labels and title






