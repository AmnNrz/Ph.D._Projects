library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(ComplexUpset)

# path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')


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

# Read raw data
Residue <- read.csv(paste0(path_to_data, 
                           "Residue.csv"),
                    header = TRUE, row.names = NULL)
Residue <- Residue[-c(1, 8)]

Soil <- read.csv(paste0(path_to_data, 
                        "Soil.csv"),
                 header = TRUE, row.names = NULL)
Soil <- Soil[-c(1, 8)]

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




# df <- Residue
# typeList <- fresh_crops
Dm <- function(df, typeList){
  D <- data.frame()
  # type <- typeList[2]
  for (type in typeList){
 
    df_filtered <- dplyr::filter(df, Type == type)
    df_filtered <- df_filtered %>% 
      pivot_wider(names_from = RWC, values_from = Reflect) 
    
    df_filtered <- as.data.frame(df_filtered)
    rownames(df_filtered) <- df_filtered$Wvl
    
    X <- df_filtered[, 4:ncol(df_filtered)]
    
    min_col <- which.min(colnames(X))
    X_wet <- X[,-min_col]
    
    D_ <- X_wet - matrix(rep(X[,min_col], ncol(X_wet)),
                        ncol = ncol(X_wet), byrow = FALSE)
    
    D_mat <- as.matrix(D_)
    D_mat <- t(D_mat)
    
    D <- rbind(D, D_mat)
    
  }
  
  return(D)
}


# df <- Residue
# typeList <- fresh_crops
# EPO Function
epo <- function(df, typeList){
  
  
  D <- as.matrix(Dm(df, typeList))
  
  # Perform SVD on t(D) %*% D
  svd_result <- svd(t(D) %*% D)
  
  U <- svd_result$u
  S <- svd_result$d
  V <- svd_result$v
  
  Vs <- V[, 1:1]
  Q <- Vs %*% t(Vs)

  P <- diag(nrow(Q)) - Q
  return(list(P=P, Q=Q))
  
}

Residue <- Residue %>% select(-Scan) 
Soil <- Soil %>% select(-Scan)

# Calculate Xsr_hat
crops <- unique(Residue$Type)
soils <- unique(Soil$Type)

# crp <- crops[1]
# sl <- soils[1]


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


results_residue <- epo(Residue, fresh_crops)
Pr <- results_residue$P
Qr <- results_residue$Q

results_soil <- epo(Soil, dark_soils)
Ps <- results_soil$P
Qs <- results_soil$Q

Xsr_transformed <- data.frame()
mix <- fresh_dark$mix[1]
for (mix in sort(fresh_dark$mix)){
  # filter mixed_original by crop and soil
  mixed_original_filtered <-
    dplyr::filter(mixed_original, Type== mix)
  

  fr <- unique(mixed_original_filtered$Fraction)[1]
  
  Xsr_HAT <- data.frame()
  for (fr in unique(mixed_original_filtered$Fraction)){
  
    Xsr <- dplyr::filter(mixed_original_filtered, Fraction == fr)
    Xsr <- Xsr %>% 
      select(-c(Scan, soil_rwc, RWC_ave)) %>% 
      
      # rwc values in Xsr would be crop rwc. Therefore, in the Xsr_transformed
      # we will have crop_rwc not soil 
      pivot_wider(names_from = crop_rwc, values_from = Reflect) 
    
    Xsr <- as.data.frame(Xsr)
    rownames(Xsr) <- Xsr$Wvl
    
    Xsr_ <- Xsr %>% 
      select(-c("Type", "Fraction", "Wvl"))
    # Xsr_ <- Xsr_ %>% 
    #   select(-as.character((min(as.numeric(names(Xsr_))))))
  
    
    Xsr_hat <- 1/2 * 
      (t(as.matrix(Xsr_)) %*% as.matrix(Ps) %*% as.matrix(Pr) + 
         t(as.matrix(Xsr_)) %*% as.matrix(Pr) %*% as.matrix(Ps))  
    
    Xsr_hat <- t(Xsr_hat)
    
    Xsr_hat <- Xsr_hat + abs(min(Xsr_hat))
    
    rownames(Xsr_hat) <- rownames(Xsr_) 
    
    Xsr_hat <- as.data.frame(Xsr_hat)
    
    Xsr_hat <-Xsr_hat %>% mutate(Wvl = rownames(Xsr_hat)) %>% 
      select("Wvl", everything())
    
    Xsr_hat <- as_tibble(Xsr_hat)
    
    Xsr_hat <- Xsr_hat %>% 
      reshape2::melt(.,id = "Wvl") %>% 
      rename(RWC = variable, Reflect = value)
      # pivot_longer(cols = names(Xsr_hat[,-1]), names_to = 'RWC',
      #              values_to = 'Reflect') 
    Xsr_hat <- cbind(Fraction = Xsr$Fraction[1], Xsr_hat) 
    Xsr_hat <- cbind(Mix = Xsr$Type[1], Xsr_hat)  
    
    Xsr_HAT <- rbind(Xsr_HAT, Xsr_hat)
  }
  Xsr_transformed <- rbind(Xsr_transformed, Xsr_HAT)
}

write.csv(Xsr_transformed, file = paste0(path_to_data, "Xsr_Transformed_.csv"),
          row.names = FALSE)


mixed_original <- mixed_original %>%
  rename(Mix = Type)

write.csv(mixed_original, file = paste0(path_to_data, "Xsr_Original.csv"),
          row.names = FALSE)




#plot heatmap of Q
row.names(Qs) <- Xsr$Wvl
colnames(Qs) <- Xsr$Wvl

df <- as.data.frame(as.table(as.matrix(Qr)))

viridis_colors <- viridis(100)

n_cols <- length(colnames(Qr))
n_rows <- length(rownames(Qr))
seq_x <- if (n_cols > 1) seq(1, n_cols, by = 200) else 1
seq_y <- if (n_rows > 1) seq(1, n_rows, by = 200) else 1
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



