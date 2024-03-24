library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(ComplexUpset)
library(MASS)


setwd(paste0('/Users/aminnorouzi/Documents/GitHub/spectroscopy_paper/',
             'Codes/10nm_resolution/Correct_EPO'))
# setwd(paste0('/home/amnnrz/Documents/GitHub/',
#              'spectroscopy_paper/Codes/10nm_resolution/individual/'))
source("epo_module.R")



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

mixed_original <- read.csv(paste0(path_to_data, 'mixed_original.csv'),
                           check.names = FALSE)


mixed_original$Type <- paste0(
  mixed_original$Crop, "_", mixed_original$Soil)

mixed_original <- mixed_original %>%
  mutate(RWC_ave = (
    mixed_original$crop_rwc + mixed_original$soil_rwc) / 2) 
mixed_original$Scan <- mixed_original$Scan

mixed_original <- mixed_original %>%
  dplyr::select(-Fraction, everything(), Fraction) %>%
  dplyr::select(c("crop_rwc", "soil_rwc", "Scan", 6:ncol(mixed_original))) %>%
  dplyr::select("Type", "RWC_ave", "Fraction", "Scan", everything())


mixed_original <- mixed_original %>% 
  pivot_longer(cols = '500':names(mixed_original)[ncol(mixed_original)],
               names_to = 'Wvl',
               values_to = 'Reflect')

# Read raw data
Residue <- read.csv(paste0(path_to_data, 
                           "Residue.csv"),
                    header = TRUE, row.names = NULL)
Residue <- Residue[-c(1, 8)]
Residue <- dplyr::filter(Residue, Wvl >=500)


Soil <- read.csv(paste0(path_to_data, 
                        "Soil.csv"),
                 header = TRUE, row.names = NULL)
Soil <- Soil[-c(1, 8)]
Soil <- dplyr::filter(Soil, Wvl >=500)


Residue <- Residue %>%
  rename(Type = Soil)

Soil <- Soil %>%
  rename(Type = Soil)

Residue <- Residue %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))


# Select common Wvls
Residue <- Residue[Residue$Wvl %in% Soil$Wvl, ]
Soil <- Soil[Soil$Wvl %in% Residue$Wvl, ]
length(unique(Residue$Wvl))
length(unique(Soil$Wvl))

Residue <- Residue %>%  dplyr::select(-Scan) 
Soil <- Soil %>%  dplyr::select(-Scan)


crops <- unique(Residue$Type)
soils <- unique(Soil$Type)

# crp <- crops[1]
# sl <- soils[1]


fresh_crops <- c("Canola", "Garbanzo Beans", "Peas",
                 "Wheat Norwest Duet", "Wheat Pritchett")

weathered_crops <- c("Weathered Canola",  "Weathered Wheat")

dark_soils <- c("Bagdad", "Mondovi 1", "Athena")

light_soils <- c("Benwy", "Shano", "Lance")


all_all <- expand.grid(crops, soils)
all_all$mix <- paste(all_all$Var1, all_all$Var2, sep = "_")
all_all <- all_all %>%  dplyr::select(-c(1,2))

fresh_dark <- expand.grid(fresh_crops, dark_soils)
fresh_dark$mix <- paste(fresh_dark$Var1, fresh_dark$Var2, sep = "_")
fresh_dark <- fresh_dark %>%  dplyr::select(-c(1,2))

fresh_light <- expand.grid(fresh_crops, light_soils)
fresh_light$mix <- paste(fresh_light$Var1, fresh_light$Var2, sep = "_")
fresh_light <- fresh_light %>%  dplyr::select(-c(1,2))

weathered_light <- expand.grid(weathered_crops, light_soils)
weathered_light$mix <- paste(weathered_light$Var1, weathered_light$Var2, sep = "_")
weathered_light <- weathered_light %>%  dplyr::select(-c(1,2))

weathered_dark <- expand.grid(weathered_crops, dark_soils)
weathered_dark$mix <- paste(weathered_dark$Var1, weathered_dark$Var2, sep = "_")
weathered_dark <- weathered_dark %>%  dplyr::select(-c(1,2))


# RWC_common <- function(Residue, Soil, mixed_original, crop_group, soil_group){
#   Res_commonRWC_df <- data.frame()
#   Soil_commonRWC_df <- data.frame()
#   for (crp in crop_group){
#     for (sl in soil_group){
#       Res_rwc_filtered <- dplyr::filter(Residue, Type==crp)
#       Soil_rwc_filtered <- dplyr::filter(Soil, Type==sl)
#       mixed_original_filtered <- 
#         dplyr::filter(mixed_original, Type== paste0(crp, "_", sl))
#       # print(length(unique(mixed_original_filtered$crop_rwc)))
#       # print(length(unique(mixed_original_filtered$soil_rwc)))
#       
#       Res_rwc_filtered <- Res_rwc_filtered[
#         Res_rwc_filtered$RWC %in% mixed_original_filtered$crop_rwc, 
#       ]
#       
#       Soil_rwc_filtered <- Soil_rwc_filtered[
#         Soil_rwc_filtered$RWC %in% mixed_original_filtered$soil_rwc, 
#       ]
#       # print(unique(Res_rwc_filtered$RWC))
#       print(unique(Soil_rwc_filtered$RWC))
#       
#       Res_commonRWC_df <- rbind(Res_commonRWC_df, Res_rwc_filtered)
#       Soil_commonRWC_df <- rbind(Soil_commonRWC_df, Soil_rwc_filtered)
#       
# 
#     }
#   }
#   return(list(df1 = Res_commonRWC_df, df2 = Soil_commonRWC_df))
# }

crop_group <- crops
soil_group <- soils
mix_group <- all_all
mix_group_name <- "allCrop_allSoils"

# crop_group <- fresh_crops
# soil_group <- dark_soils
# mix_group <- fresh_dark
# mix_group_name <- "fresh_dark"

# crop_group <- fresh_crops
# soil_group <- light_soils
# mix_group <- fresh_light
# mix_group_name <- "fresh_light"

# crop_group <- weathered_crops
# soil_group <- dark_soils
# mix_group <- weathered_dark
# mix_group_name <- "weathered_dark"

# crop_group <- weathered_crops
# soil_group <- light_soils
# mix_group <- weathered_light
# mix_group_name <- "weathered_light"

# result <- RWC_common(Residue, Soil, mixed_original, crop_group, soil_group)
# Res_commonRWC_df <- result$df1
# # Soil_commonRWC_df <- result$df2
# print(length(unique(Res_commonRWC_df$RWC)))
# print(length(unique(Soil_commonRWC_df$RWC)))


df <- Residue
sample <- "Residue"
# df <- Soil_commonRWC_df
# sample <- "Soil"

# Res_commonRWC_df <- dplyr::filter(Res_commonRWC_df, Type == 'Canola')
results_residue <- epo_scenario2(Residue, "Residue")
Pr <- results_residue$P
Qr <- results_residue$Q

# Soil_commonRWC_df <- dplyr::filter(Soil_commonRWC_df, Type == 'Athena')

results_soil <- epo_scenario2(Soil, "Soil")
Ps <- results_soil$P
Qs <- results_soil$Q


mixed_original <- mixed_original %>%
  rename(Mix = Type)

org_mixes_filtered <- mixed_original[
  mixed_original$Mix %in% mix_group$mix, 
]



fr <- unique(org_mixes_filtered$Fraction)[1]
mix <- unique(org_mixes_filtered$Mix)[1]
Xsr_HAT <- data.frame()
for (fr in unique(org_mixes_filtered$Fraction)){
  Xsr_hat_mix <- data.frame()
  for (mix in unique(org_mixes_filtered$Mix)){
    Xsr_fr <- dplyr::filter(org_mixes_filtered, Fraction == fr)
    Xsr_mix <- dplyr::filter(Xsr_fr, Mix == mix)
    Xsr <- Xsr_mix
    # Xsr$Type <- paste0(mix_group)
    Xsr <- Xsr %>% 
      dplyr::select(-c(Scan, soil_rwc, RWC_ave)) %>% 
      
      # rwc values in Xsr would be crop rwc. Therefore, in the Xsr_transformed
      # we will have crop_rwc not soil 
      pivot_wider(names_from = crop_rwc, values_from = Reflect)
    
    Xsr <- as.data.frame(Xsr)
    rownames(Xsr) <- Xsr$Wvl
  
    Xsr_ <- Xsr %>% 
      dplyr::select(-c("Mix", "Fraction", "Wvl"))
    Xsr_ <- Xsr_[, order(as.numeric(colnames(Xsr_)))]
    # min_col <- which.min(colnames(Xsr_))
    # Xsr_ <- Xsr_[, -min_col]
    Xsr_ <- t(Xsr_)
    Xsr_hat <- 1/2 * 
      (as.matrix(Xsr_) %*% as.matrix(Ps) %*% as.matrix(Pr) + 
         as.matrix(Xsr_) %*% as.matrix(Pr) %*% as.matrix(Ps)) 
    
    # Xsr_hat <- t(Xsr_hat)
    
    # Xsr_hat <- Xsr_hat + abs(min(Xsr_hat))
    
    rownames(Xsr_hat) <- rownames(Xsr_) 
    colnames(Xsr_hat) <- colnames(Xsr_) 
    Xsr_hat <- as.data.frame(Xsr_hat)
    Xsr_hat <- as.data.frame(t(Xsr_hat))
    Xsr_hat <-Xsr_hat %>% mutate(Wvl = rownames(Xsr_hat)) %>% 
      dplyr::select("Wvl", everything())
    
    Xsr_hat <- as_tibble(Xsr_hat)
    
    Xsr_hat <- Xsr_hat %>% 
      reshape2::melt(.,id = "Wvl") %>% 
      rename(RWC = variable, Reflect = value)
    # pivot_longer(cols = names(Xsr_hat[,-1]), names_to = 'RWC',
    #              values_to = 'Reflect') 
    Xsr_hat <- cbind(Fraction = Xsr$Fraction[1], Xsr_hat) 
    Xsr_hat <- cbind(Mix = mix, Xsr_hat)
    Xsr_hat_mix <- rbind(Xsr_hat_mix, Xsr_hat)
  }
    Xsr_HAT <- rbind(Xsr_HAT, Xsr_hat_mix)
}


org_mixes_filtered <- org_mixes_filtered %>% 
  dplyr::select(-c("RWC_ave", "Scan", "soil_rwc")) %>% 
  rename(RWC = crop_rwc)


write.csv(Xsr_HAT, file = paste0(path_to_data, "Xsr_", mix_group_name, "_Transformed.csv"),
          row.names = FALSE)
write.csv(org_mixes_filtered, file = paste0(path_to_data, "Xsr_", mix_group_name, "_Original.csv"),
          row.names = FALSE)
write.csv(as.data.frame(Pr), file = paste0(path_to_data, "Pr_", mix_group_name, ".csv"),
          row.names = FALSE)
write.csv(as.data.frame(Ps), file = paste0(path_to_data, "Ps_", mix_group_name, ".csv"),
          row.names = FALSE)



#plot heatmap of Q
row.names(Pr) <- Xsr$Wvl
colnames(Pr) <- Xsr$Wvl

df <- as.data.frame(as.table(as.matrix(Pr)))

viridis_colors <- viridis(100)

n_cols <- length(colnames(Qs))
n_rows <- length(rownames(Qs))
seq_x <- if (n_cols > 1) seq(1, n_cols, by = 20) else 1
seq_y <- if (n_rows > 1) seq(1, n_rows, by = 20) else 1
df$Var2 <- as.factor(df$Var2)
df$Var1 <- as.factor(df$Var1)


ggplot(df, aes(Var2, Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    axis.text.y = element_text(angle = 0)
  ) +
  scale_x_discrete(breaks = levels(df$Var2)[seq_x]) +
  scale_y_discrete(breaks = levels(df$Var1)[seq_y])

# 
# 
# 
# 
# 
# 



