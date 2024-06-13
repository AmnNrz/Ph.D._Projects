library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#         'Projects/Spectroscopy_Paper/Data/10nm_Senarios_Wangetal_correct/')

Xsr_Original <- read.csv(paste0(path_to_data, "Xsr_Original.csv"))
Xsr_Transformed <- read.csv(paste0(path_to_data, "Xsr_Transformed.csv"))
                            
Xsr_Original <- Xsr_Original %>% 
  select(c("Mix", "Fraction", "crop_rwc", "Wvl", "Reflect")) %>% 
  rename(RWC = crop_rwc)

Xsr_Original <- Xsr_Original[names(Xsr_Transformed)]

# create dataframe for "Spectra ~ Wvl" before/after EPO plot 
Xsr_Org <- Xsr_Original
Xsr_Org$source <- 'Original'
Xsr_Trans <- Xsr_Transformed
Xsr_Trans$source <- 'EPO'

Xsr_combined <- rbind(Xsr_Org, Xsr_Trans)
write.csv(Xsr_combined, file = paste0(path_to_data, "Xsr_combined.csv"), row.names = FALSE)

# # Select required Wvl
# target_wavelengths <- c(2200, 2000, 2100, 2260, 1660, 1600, 2330)
# 
# # Calculate indices using original reflectance 
# Xsr_Original_Wvl <- Xsr_Original %>%
#   dplyr::filter(Wvl %in% target_wavelengths)

Xsr_Original_indices <- Xsr_Original %>%
  spread(Wvl, Reflect) 

select_columns_range <- function(df, start_col_name, end_col_name) {
  start_col <- which(names(df) == start_col_name)
  end_col <- which(names(df) == end_col_name)
  if (start_col == 0 || end_col == 0) {
    stop("One of the specified column names does not exist in the dataframe.")
  }
  selected_df <- df[, start_col:end_col]
  return(selected_df)
}

# # CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2250`) - CAI$`2090`)
# # CAI$SINDRI <- (CAI$`2200` - CAI$`2260`) / (CAI$`2200` + CAI$`2260`)
# CAI$R2220_2260 <-  rowMeans(select_columns_range(CAI, '2240', '2260'))
# CAI$R2260_2280 <-  rowMeans(select_columns_range(CAI, '2295', '2330'))
# CAI$SINDRI <- (CAI$R2220_2260 - CAI$R2260_2280) / (CAI$R2220_2260 + CAI$R2260_2280)
# # CAI$NDTI <- (CAI$`1660` - CAI$`2330`) / (CAI$`1660` + CAI$`2330`)
# CAI$R1660_1690 <-  rowMeans(select_columns_range(CAI, '1660', '1690'))
# CAI$R2220_2280 <-  rowMeans(select_columns_range(CAI, '2220', '2280'))
# CAI$NDTI <- (CAI$R1660_1690 - CAI$R2220_2280) / (CAI$R1660_1690 + CAI$R2220_2280)

Xsr_Original_indices$CAI <- 
  (0.5 * (Xsr_Original_indices$`2000` + Xsr_Original_indices$`2250`) - 
     Xsr_Original_indices$`2090`)


Xsr_Original_indices$R2220_2260 <-  
  rowMeans(select_columns_range(Xsr_Original_indices, '2240', '2260'))
Xsr_Original_indices$R2260_2280 <-  
  rowMeans(select_columns_range(Xsr_Original_indices, '2290', '2330'))
Xsr_Original_indices$SINDRI <- 
  (Xsr_Original_indices$R2220_2260 - Xsr_Original_indices$R2260_2280) / 
  (Xsr_Original_indices$R2220_2260 + Xsr_Original_indices$R2260_2280)



Xsr_Original_indices$R1660_1690 <-  
  rowMeans(select_columns_range(Xsr_Original_indices, '1660', '1690'))
Xsr_Original_indices$R2220_2280 <-  
  rowMeans(select_columns_range(Xsr_Original_indices, '2220', '2280'))
Xsr_Original_indices$NDTI <- 
  (Xsr_Original_indices$R1660_1690 - Xsr_Original_indices$R2220_2280) / 
  (Xsr_Original_indices$R1660_1690 + Xsr_Original_indices$R2220_2280)

Xsr_Original_indices$R2220 <- Xsr_Original_indices$`2250`/Xsr_Original_indices$`2000`
Xsr_Original_indices$R1620 <- Xsr_Original_indices$`1600`/Xsr_Original_indices$`2000`
Xsr_Original_indices$RSWIR <- Xsr_Original_indices$`1660`/Xsr_Original_indices$`R2260_2280`
Xsr_Original_indices$ROLI <- Xsr_Original_indices$`1660`/Xsr_Original_indices$`R2220_2280`




# # Calculate indices using transformed reflectance 
# Xsr_Transformed_Wvl <- Xsr_Transformed %>%
#   dplyr::filter(Wvl %in% target_wavelengths)

Xsr_Transformed_indices <- Xsr_Transformed %>%
  spread(Wvl, Reflect) 

Xsr_Transformed_indices$CAI <- 
  (0.5 * (Xsr_Transformed_indices$`2000` + Xsr_Transformed_indices$`2250`) - 
     Xsr_Transformed_indices$`2090`)

Xsr_Transformed_indices$R2220_2260 <-  
  rowMeans(select_columns_range(Xsr_Transformed_indices, '2240', '2260'))
Xsr_Transformed_indices$R2260_2280 <-  
  rowMeans(select_columns_range(Xsr_Transformed_indices, '2290', '2330'))
Xsr_Transformed_indices$SINDRI <- 
  (Xsr_Transformed_indices$R2220_2260 - Xsr_Transformed_indices$R2260_2280) / 
  (Xsr_Transformed_indices$R2220_2260 + Xsr_Transformed_indices$R2260_2280)







Xsr_Transformed_indices$R1660_1690 <-  
  rowMeans(select_columns_range(Xsr_Transformed_indices, '1660', '1690'))
Xsr_Transformed_indices$R2220_2280 <-  
  rowMeans(select_columns_range(Xsr_Transformed_indices, '2220', '2280'))
Xsr_Transformed_indices$NDTI <- 
  (Xsr_Transformed_indices$R1660_1690 - Xsr_Transformed_indices$R2220_2280) / 
  (Xsr_Transformed_indices$R1660_1690 + Xsr_Transformed_indices$R2220_2280)


Xsr_Transformed_indices$R2220 <- Xsr_Transformed_indices$`2250`/Xsr_Transformed_indices$`2000`
Xsr_Transformed_indices$R1620 <- Xsr_Transformed_indices$`1600`/Xsr_Transformed_indices$`2000`
Xsr_Transformed_indices$RSWIR <- Xsr_Transformed_indices$`1660`/Xsr_Transformed_indices$`R2260_2280`
Xsr_Transformed_indices$ROLI <- Xsr_Transformed_indices$`1660`/Xsr_Transformed_indices$`R2220_2280`




Xsr_Original_indices$source <- 'Original'
Xsr_Transformed_indices$source <- 'EPO'

Xsr_Original_Transformed_indices <- rbind(Xsr_Original_indices, Xsr_Transformed_indices)


write.csv(Xsr_Original_Transformed_indices, file = paste0(path_to_data, "Xsr_Original_Transformed_indices.csv"), 
          row.names = FALSE)





