library(tidyverse)
library(dplyr)
library(ggplot2)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# 
# path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/')

Residue_Median <- read.csv(paste0(path_to_data, 
                                  "Residue.csv"),
                           header = TRUE, row.names = NULL)
Residue_Median <- Residue_Median[-c(1, 8)]
Residue_Median <- dplyr::filter(Residue_Median, Wvl >=500)

Soil_Median <- read.csv(paste0(path_to_data, 
                               "Soil.csv"),
                        header = TRUE, row.names = NULL)
Soil_Median <- Soil_Median[-c(1, 8)]
Soil_Median <- dplyr::filter(Soil_Median, Wvl >=500)

Residue_Median <- Residue_Median %>%
  rename(Type = Soil)

Soil_Median <- Soil_Median %>%
  rename(Type = Soil)

Residue_Median <- Residue_Median %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))

Residue <- Residue_Median
Soil <- Soil_Median

res_wide <- Residue %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 
soil_wide <- Soil %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 

###############################################################
###############################################################
# Check common RWC ranges using histogram of RWC
res_ = res_wide[, 1:4]
soil_ = soil_wide[, 1:4]

res_soil_ <- rbind(res_, soil_)

ggplot(res_soil_, aes(x=RWC)) + 
  geom_histogram(breaks=seq(0, 1, by=0.1), fill="lightblue", color="black",
                 alpha=0.7) +
  facet_wrap(~Type, scales="free_y") + 
  labs(title="Distribution of RWC across different soils and crop residues",
       x="RWC", 
       y="Count") +
  scale_x_continuous(breaks=seq(0, 1, by=0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################
###############################################################
common_cols <- intersect(names(res_wide), names(soil_wide))
res_wide <- res_wide[, common_cols]

cropRWC <- res_wide %>%
  dplyr::select("Type", "RWC") %>% 
  rename(Crop = Type, Crop_RWC = RWC)
soilRWC <- soil_wide %>%
  dplyr::select("Type", "RWC") %>% 
  rename(Soil = Type, Soil_RWC = RWC)

crops <-  unique(cropRWC$Crop)
soils <- unique(soilRWC$Soil)

commonRWC_df <- data.frame()
for (crp in crops){
  for (sl in soils){
    # Filter by one crop and one soil
    cropRWC_filtered <- cropRWC %>% 
      filter(Crop == crp)
    soilRWC_filtered <- soilRWC %>% 
      filter(Soil == sl)
    
    # Cross join
    cross_df <- cropRWC_filtered %>%
      crossing(soilRWC_filtered) %>% 
      mutate(Difference = abs(Crop_RWC - Soil_RWC))
    
    df <- data.frame()
    for (rwc in unique(cropRWC_filtered$Crop_RWC)){
      
      rwc_df <- dplyr::filter(cross_df, Crop_RWC == rwc)
      selected_row <- rwc_df[which.min(rwc_df$Difference),]
      
      df <- rbind(df, selected_row)
    }
    
    commonRWC_df <- rbind(commonRWC_df, df)
}
}

residue_df <- dplyr::filter(res_wide, Sample == 'Residue')
soil_df <- dplyr::filter(soil_wide, Sample == 'Soil')

###############################################################
###############################################################
#####
                # mix one crop, soil and RWC range 
#####
###############################################################
###############################################################

# # Filter by one crop and one soil
# crop_filtered <- residue_df %>% 
#   filter(Type == 'Peas')
# soil_filtered <- soil_df %>% 
#   filter(Type == 'Athena')
# 
# # filter by one RWC range
# rwc_crop <- dplyr::filter(crop_filtered, RWC_range == '0-0.25')
# rwc_soil <- dplyr::filter(soil_filtered, RWC_range == '0-0.25')
# 
# cropScans <- unique(rwc_crop$Scan)
# soilScans <- unique(rwc_soil$Scan)
# 
# mixed_dataframe <- data.frame()
# 
# for (i in cropScans){
#   for (j in soilScans){
#     
#     fractions <- sort(runif(10, min = 0, max = 1))
#     
#     cropReflect <- dplyr::filter(rwc_crop, Scan == i) %>% 
#       select("500":ncol(rwc_crop))
#     
#     soilReflect <- dplyr::filter(rwc_soil, Scan == j)%>% 
#       select("500":ncol(rwc_soil))
#     
#     Rr <- lapply(fractions, function(fr) as.numeric(cropReflect) * fr)
#     Rs <- lapply(fractions, function(fr) as.numeric(soilReflect) * (1-fr))
#     
#     Rmix <- mapply(FUN = `+`, Rr, Rs, SIMPLIFY = FALSE)
#     
#     mixed_df <- as.data.frame(do.call(rbind, Rmix))
#     colnames(mixed_df) <- names(rwc_crop)[6:ncol(rwc_crop)]
#     
#     mixed_df$cropType <- rwc_crop$Type[1]
#     mixed_df$soilType <- rwc_soil$Type[1]
#     mixed_df$cropRWC <- rwc_crop$RWC[1]
#     mixed_df$soilRWC <- rwc_soil$RWC[1]
#     mixed_df$Fr <- sort(runif(10, min = 0, max = 1))
#     mixed_df$cropScan <- i
#     mixed_df$soilScan <- j
#     
#     mixed_df <- mixed_df %>% 
#       select("cropType", "soilType", "cropRWC", "soilRWC", "Fr", "cropScan", 
#       "soilScan", everything())
#     
#     mixed_dataframe <- rbind(mixed_dataframe, mixed_df)
#   }
# }

###############################################################
###############################################################
#####
              # mix all crops, soils and RWC ranges 
#####
###############################################################
###############################################################
# Filter by one crop and one soil
crops = unique(res_wide$Type)
soils = unique(soil_wide$Type)

fractions <- seq(0, 1, by = 0.1)
mixed_dataframe <- data.frame()
for (crp in crops){
  for (sl in soils){
    print(paste0(crp, "_", sl))
    
    # filter commonRWC_df by crop and soil
    filtered_commonRWC_df <- commonRWC_df %>% 
      filter(Crop == crp & Soil == sl)
    filtered_commonRWC_df <- filtered_commonRWC_df %>%
      filter(!duplicated(Soil_RWC)) %>% 
      filter(!duplicated(Crop_RWC))

    crop_filtered <- residue_df %>% 
      filter(Type == crp) %>% 
      filter(RWC %in% filtered_commonRWC_df$Crop_RWC)
      
    soil_filtered <- soil_df %>% 
      filter(Type == sl) %>% 
      filter(RWC %in% filtered_commonRWC_df$Soil_RWC)
          
    cropReflect <- crop_filtered %>%  dplyr::select("500":ncol(crop_filtered))
    
    soilReflect <- soil_filtered %>%  dplyr::select("500":ncol(soil_filtered))
          
    Rmix <- lapply(
      fractions, function(fr) {
        df <- (cropReflect * fr) + (soilReflect * (1-fr))
        df <- cbind(Fraction = fr, df)
        df <- cbind(crop_rwc = crop_filtered$RWC, df)
        df <- cbind(
          Scan = paste0(crop_filtered$Scan, "_", soil_filtered$Scan), df)
        df <- cbind(soil_rwc = soil_filtered$RWC, df)
        return(df)
      })
          
    mixed_df <- data.frame()
    mixed_df <- as.data.frame(do.call(rbind, Rmix))
    mixed_df <- cbind(Crop = crp, mixed_df)
    mixed_df <- cbind(Soil = sl, mixed_df)
    
    mixed_dataframe <- rbind(
      mixed_dataframe, mixed_df)
  }
}

# Standard Normal Variate (SNV) Transformation
snv_transform <- function(matrix) {
  snv_transformed <- apply(matrix, 1, function(row) {
    row_mean <- mean(row)
    row_sd <- sd(row)
    (row - row_mean) / row_sd
  })
  return(t(snv_transformed)) # Transpose to maintain original orientation
}

# spectra_matrix <- mixed_dataframe %>% dplyr::select("1500":ncol(mixed_dataframe))
# spectra_matrix <- as.matrix(spectra_matrix)
# snv_transformed_matrix <- snv_transform(spectra_matrix)
# mixed_dataframe_firstCols <- mixed_dataframe %>%  dplyr::select("Soil":6)
# mixed_dataframe <- cbind(mixed_dataframe_firstCols, snv_transformed_matrix)

write.csv(mixed_dataframe, paste0(path_to_data, "mixed_original.csv"), row.names = FALSE)
