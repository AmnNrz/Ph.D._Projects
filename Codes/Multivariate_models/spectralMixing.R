library(tidyverse)
library(dplyr)
library(ggplot2)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/',
                       'multivariate_model/')

Residue_Median <- read.csv(paste0(path_to_data, 
                                  "Residue.csv"),
                           header = TRUE, row.names = NULL)
Residue_Median <- Residue_Median[-c(1, 8)]

Soil_Median <- read.csv(paste0(path_to_data, 
                               "Soil.csv"),
                        header = TRUE, row.names = NULL)
Soil_Median <- Soil_Median[-c(1, 8)]

Residue_Median <- Residue_Median %>%
  rename(Type = Soil)

Soil_Median <- Soil_Median %>%
  rename(Type = Soil)

Residue_Median <- Residue_Median %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))

Residue <- Residue_Median
Soil <- Soil_Median
uniq_wvl <- unique(Soil$Wvl)
Residue <- dplyr::filter(Residue, Wvl %in% uniq_wvl)

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
library(dplyr)
library(tidyr)

# Assuming df1 and df2 are your data frames
df1 <- Residue
df2 <- Soil

library(dplyr)

# Preparing df1 and df2 by renaming the columns to indicate their origin
df1 <- mutate(df1, crop_RWC = RWC, crop_Reflect = Reflect, crop_Type = Type) %>%
  select(-RWC, -Reflect, -Type)
df2 <- mutate(df2, soil_RWC = RWC, soil_Reflect = Reflect, soil_Type = Type) %>%
  select(-RWC, -Reflect, -Type)

# Generating a unique identifier for each 'Type' in df1 and df2, if not already done
df1 <- df1 %>% mutate(crop_Type_id = paste0("crop_", crop_Type))
df2 <- df2 %>% mutate(soil_Type_id = paste0("soil_", soil_Type))

# Manually creating combinations of df1 and df2 based on matching 'Wvl' values
# This involves a cross-join (Cartesian product) followed by filtering for matching 'Wvl'
merged_df <- full_join(df1, df2, by = "Wvl") %>%
  mutate(Mix = paste(crop_Type, soil_Type, sep = "_")) %>%
  select(Mix, crop_RWC, soil_RWC, crop_Reflect, soil_Reflect, Wvl)


# Creating a dataframe with the sequence
seq_df <- data.frame(Fr = seq(0, 1, by = 0.1))

# Expanding the original dataframe
expanded_df <- merged_df %>%
  # Create an identifier to ensure each row is repeated independently
  mutate(row_id = row_number()) %>%
  # Cross-join with seq_df to repeat each row for each Fr value
  expand_grid(seq_df) %>%
  # Remove the temporary row identifier
  select(-row_id)

expanded_df$mixed_reflect <- expanded_df$soil_Reflect * (1-expanded_df$Fr) +
                                      expanded_df$crop_Reflect * expanded_df$Fr
expanded_df$mixed_RWC <- expanded_df$soil_RWC * (1-expanded_df$Fr) +
                                      expanded_df$crop_RWC * expanded_df$Fr
# expanded_df$mixed_RWC <- paste(expanded_df$crop_RWC, expanded_df$soil_RWC, 
                               # sep = "_")
expanded_df <- expanded_df[order(expanded_df$Mix, expanded_df$Fr), ]

write.csv(expanded_df, paste0(path_to_data, "mixed_reflect_df.csv"), row.names = FALSE)


# # Filter by one crop and one soil
# crops = unique(res_wide$Type)
# soils = unique(soil_wide$Type)
# 
# ############### mixe spectra of all RWC
# 
# fractions <- seq(0, 1, by = 0.1)
# mixed_dataframe <- data.frame()
# for (crp in crops){
#   for (sl in soils){
#     print(paste0(crp, "_", sl))
#     
#  
#     cropReflect <- crop_filtered %>%  dplyr::select("500":ncol(crop_filtered))
#     
#     soilReflect <- soil_filtered %>%  dplyr::select("500":ncol(soil_filtered))
#     
#     Rmix <- lapply(
#       fractions, function(fr) {
#         df <- (cropReflect * fr) + (soilReflect * (1-fr))
#         df <- cbind(Fraction = fr, df)
#         df <- cbind(crop_rwc = crop_filtered$RWC, df)
#         df <- cbind(
#           Scan = paste0(crop_filtered$Scan, "_", soil_filtered$Scan), df)
#         df <- cbind(soil_rwc = soil_filtered$RWC, df)
#         return(df)
#       })
#     
#     mixed_df <- data.frame()
#     mixed_df <- as.data.frame(do.call(rbind, Rmix))
#     mixed_df <- cbind(Crop = crp, mixed_df)
#     mixed_df <- cbind(Soil = sl, mixed_df)
#     
#     mixed_dataframe <- rbind(
#       mixed_dataframe, mixed_df)
#   }
# }
# 
# write.csv(mixed_dataframe, paste0(path_to_data, "mixed_original.csv"), row.names = FALSE)
# 
# 
# ############### mixed spectra of common RWC
# 
# fractions <- seq(0, 1, by = 0.1)
# mixed_dataframe <- data.frame()
# for (crp in crops){
#   for (sl in soils){
#     print(paste0(crp, "_", sl))
#     
#     # filter commonRWC_df by crop and soil
#     filtered_commonRWC_df <- commonRWC_df %>% 
#       filter(Crop == crp & Soil == sl)
#     filtered_commonRWC_df <- filtered_commonRWC_df %>%
#       filter(!duplicated(Soil_RWC)) %>% 
#       filter(!duplicated(Crop_RWC))
# 
#     crop_filtered <- residue_df %>% 
#       filter(Type == crp) %>% 
#       filter(RWC %in% filtered_commonRWC_df$Crop_RWC)
#       
#     soil_filtered <- soil_df %>% 
#       filter(Type == sl) %>% 
#       filter(RWC %in% filtered_commonRWC_df$Soil_RWC)
#           
#     cropReflect <- crop_filtered %>%  dplyr::select("500":ncol(crop_filtered))
#     
#     soilReflect <- soil_filtered %>%  dplyr::select("500":ncol(soil_filtered))
#           
#     Rmix <- lapply(
#       fractions, function(fr) {
#         df <- (cropReflect * fr) + (soilReflect * (1-fr))
#         df <- cbind(Fraction = fr, df)
#         df <- cbind(crop_rwc = crop_filtered$RWC, df)
#         df <- cbind(
#           Scan = paste0(crop_filtered$Scan, "_", soil_filtered$Scan), df)
#         df <- cbind(soil_rwc = soil_filtered$RWC, df)
#         return(df)
#       })
#           
#     mixed_df <- data.frame()
#     mixed_df <- as.data.frame(do.call(rbind, Rmix))
#     mixed_df <- cbind(Crop = crp, mixed_df)
#     mixed_df <- cbind(Soil = sl, mixed_df)
#     
#     mixed_dataframe <- rbind(
#       mixed_dataframe, mixed_df)
#   }
# }
# 
# 
# 
# write.csv(mixed_dataframe, paste0(path_to_data, "mixed_original.csv"), row.names = FALSE)
