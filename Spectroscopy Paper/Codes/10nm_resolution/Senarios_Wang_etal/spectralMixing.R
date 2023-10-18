library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)',
                       '/Ph.D/Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal/')


Residue <- read.csv(paste0(path_to_data, "Residue.csv"))
Residue <- Residue[, -c(1, ncol(Residue))] %>% 
  mutate(Sample = ifelse(is.character('Crop Residue'), 'Residue', Sample))

Soil <- read.csv(paste0(path_to_data, "Soil.csv"))

Soil <- Soil[, -c(1, ncol(Soil))]

res_wide <- Residue %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 
soil_wide <- Soil %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 

res_wide <- res_wide %>% rename(Type = Crop)
soil_wide <- soil_wide %>% rename(Type = Soil)

###############################################################
###############################################################
# Check common RWC ranges using hisotogram of RWC
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

df <- rbind(res_wide, soil_wide)

df <- df %>%
  mutate(`RWC_range` = case_when(
    RWC >= 0.0 & RWC <= 0.25 ~ "0-0.25",
    RWC >= 0.48 & RWC <= 0.65 ~ "0.48-0.65",
    RWC >= 0.80 & RWC <= 1 ~ "0.85-1",
    TRUE ~ "Other"
  )) %>%
  select(1:3, "RWC_range", everything())

df <- df %>%
  filter(`RWC_range` != "Other")

residue_df <- dplyr::filter(df, Sample == 'Residue')
soil_df <- dplyr::filter(df, Sample == 'Soil')



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

mixed_dataframe <- data.frame()
for (crp in crops){
  for (sl in soils){
    print(paste0(crp, "_", sl))
    fractions <- sort(runif(10, min = 0, max = 1))
    
    # Filter by one crop and one soil
    crop_filtered <- residue_df %>% 
      filter(Type == crp)
    soil_filtered <- soil_df %>% 
      filter(Type == sl)
    
    mixed_dataframe_rwcLevels <- data.frame()
    common_RWC <- intersect(unique(crop_filtered$RWC_range),
                            unique(soil_filtered$RWC_range))
    for (rwc_ in common_RWC){
      print(rwc_)
      # filter by one RWC range
      rwc_crop <- dplyr::filter(crop_filtered, RWC_range == rwc_)
      rwc_soil <- dplyr::filter(soil_filtered, RWC_range == rwc_)
      
      cropScans <- unique(rwc_crop$Scan)
      soilScans <- unique(rwc_soil$Scan)
      
      
      mixed_dataframe_scans<- data.frame()
      for (i in cropScans){
        for (j in soilScans){
          print(paste0(i, "_", j))
          
          cropReflect <- dplyr::filter(rwc_crop, Scan == i) %>% 
            select("500":ncol(rwc_crop))
          
          soilReflect <- dplyr::filter(rwc_soil, Scan == j)%>% 
            select("500":ncol(rwc_soil))
          
          Rr <- lapply(fractions, function(fr) as.numeric(cropReflect) * fr)
          Rs <- lapply(fractions, function(fr) as.numeric(soilReflect) * (1-fr))
          
          Rmix <- mapply(FUN = `+`, Rr, Rs, SIMPLIFY = FALSE)
          
          mixed_df <- data.frame()
          mixed_df <- as.data.frame(do.call(rbind, Rmix))
          colnames(mixed_df) <- names(rwc_crop)[6:ncol(rwc_crop)]
          
          mixed_df$cropType <- rwc_crop$Type[1]
          mixed_df$soilType <- rwc_soil$Type[1]
          
          rwc_crp <- dplyr::filter(rwc_crop, Scan == i)$RWC[1]
          rwc_sl <- dplyr::filter(rwc_soil, Scan == j)$RWC[1]
          mixed_df$cropRWC <- rwc_crp
          mixed_df$soilRWC <- rwc_sl
          
          mixed_df$Fr <- sort(fractions)
          mixed_df$cropScan <- i
          mixed_df$soilScan <- j
          
          mixed_df <- mixed_df %>% 
            select("cropType", "soilType", "cropRWC", "soilRWC", "Fr", "cropScan", 
                   "soilScan", everything())
          
          mixed_dataframe_scans <- rbind(mixed_dataframe_scans, mixed_df)
        }
      }
      mixed_dataframe_rwcLevels <- rbind(
          mixed_dataframe_rwcLevels, mixed_dataframe_scans)
    }
    mixed_dataframe <- rbind(
      mixed_dataframe, mixed_dataframe_rwcLevels)
  }
}



write.csv(mixed_dataframe, paste0(path_to_data, "mixed_original.csv"), row.names = FALSE)
