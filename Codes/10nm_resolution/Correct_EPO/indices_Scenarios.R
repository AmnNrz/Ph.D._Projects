library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

Xsr_Original <- read.csv(paste0(path_to_data, "Xsr_allCrop_allSoils_Original.csv"))
Xsr_Transformed <- read.csv(paste0(path_to_data, "Xsr_allCrop_allSoils_Transformed.csv"))

Xsr_Original <- Xsr_Original[names(Xsr_Transformed)]

# create dataframe for "Spectra ~ Wvl" before/after EPO plot 
Xsr_Org <- Xsr_Original
Xsr_Org$source <- 'Original'
Xsr_Trans <- Xsr_Transformed
Xsr_Trans$source <- 'EPO'

Xsr_combined <- rbind(Xsr_Org, Xsr_Trans)
write.csv(Xsr_combined, file = paste0(path_to_data, "Xsr_combined_allCrop_allSoils.csv"), row.names = FALSE)

select_columns_range <- function(df, start_col_name, end_col_name) {
  start_col <- which(names(df) == start_col_name)
  end_col <- which(names(df) == end_col_name)
  if (start_col == 0 || end_col == 0) {
    stop("One of the specified column names does not exist in the dataframe.")
  }
  selected_df <- df[, start_col:end_col]
  return(selected_df)
}

# Function to replace each vector with its mean
replace_vector_with_mean <- function(x) {
  sapply(x, function(v) mean(unlist(v)))
}

# Xsr_Original_indices <- Xsr_Original %>%
#   spread(Wvl, Reflect) 

Xsr_Original_indices <- Xsr_Original %>%
  pivot_wider(names_from = Wvl, values_from = Reflect)
Xsr_org_first3 <- Xsr_Original_indices[, 1:3]
Xsr_org_rest <- Xsr_Original_indices[, 4:ncol(Xsr_Original_indices)]
Xsr_wide <- apply(Xsr_org_rest, 2, replace_vector_with_mean)
Xsr_Original_indices <- cbind(Xsr_org_first3, Xsr_wide)

# Xsr_Original_indices$CAI <-
#   (0.5 * (Xsr_Original_indices$`2200` + Xsr_Original_indices$`2250`) -
#      Xsr_Original_indices$`2140`)
Xsr_Original_indices$CAI <-
  (0.5 * (Xsr_Original_indices$`2030` + Xsr_Original_indices$`2140`) -
     Xsr_Original_indices$`2070`)

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

Xsr_Transformed_indices <- Xsr_Transformed %>%
  spread(Wvl, Reflect) 

# Xsr_Transformed_indices$CAI <-
#   (0.5 * (Xsr_Transformed_indices$`2000` + Xsr_Transformed_indices$`2250`) -
#      Xsr_Transformed_indices$`2140`)
Xsr_Transformed_indices$CAI <-
  (0.5 * (Xsr_Transformed_indices$`2030` + Xsr_Transformed_indices$`2140`) -
     Xsr_Transformed_indices$`2070`)


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
  rowMeans(select_columns_range(Xsr_Transformed_indices, '2230', '2200'))
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

write.csv(Xsr_Original_Transformed_indices, file = paste0(path_to_data, "Xsr_Original_Transformed_indices_allCrop_allSoils.csv"), 
          row.names = FALSE)
