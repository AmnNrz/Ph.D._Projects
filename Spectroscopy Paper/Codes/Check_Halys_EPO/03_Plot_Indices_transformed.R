## ---------------------------
##
## Script name: Create water indices
##
## Purpose of script: Filter wavelength which are used for indices like CAI, SINDRI, NDTI and create plots
##
## Author: Siddharth Chaudhary
##
## Date Created: 2022-08-26
##
## Copyright (c) Siddharth Chaudhary, 2022
## Email: siddharth.chaudhary@wsu.edu
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)

# Set the working directory
path_to_data = paste0("/Users/aminnorouzi/Library/CloudStorage/",
                      "OneDrive-WashingtonStateUniversity(email.wsu.edu)/", 
                      "Ph.D/Projects/Spectroscopy_Paper/Data/Halys/")

path_to_plots = paste0("/Users/aminnorouzi/Library/CloudStorage/",
                       "OneDrive-WashingtonStateUniversity(email.wsu.edu)/",
                       "Ph.D/Projects/Spectroscopy_Paper/Plots/Halys/")


Residue_Median <- read.csv(paste0(path_to_data, "Residue.csv"))
Residue_Median <- Residue_Median[-c(1, 7)]
Residue_Median$Sample <- gsub("Crop Residue", "Residue", Residue_Median$Sample)
Residue_Median <- dplyr::rename(Residue_Median, Type_Name = Soil)

Soil_Median <- read.csv(paste0(path_to_data, "Soil.csv"))
Soil_Median <- dplyr::rename(Soil_Median, Type_Name = Soil)
Soil_Median <- Soil_Median[-c(1, 7)]

Residue_Median <- Residue_Median[Residue_Median$Wvl > 1500, ]
Soil_Median <- Soil_Median[Soil_Median$Wvl > 1500, ]

# Rename Type_Name to Crop
colnames(Residue_Median)[colnames(Residue_Median) == "Type_Name"] <- "Crop"

# Similar to the previous method, you can use colnames() to rename a column
colnames(Soil_Median)[colnames(Soil_Median) == "Type_Name"] <- "Crop"

                    
## Renaming the column to Reflectance
colnames(Residue_Median)[6] <- "Reflectance"

CAI <- Residue_Median %>%
  dplyr::filter(Wvl == 2200 | Wvl == 2000 | Wvl == 2100 | Wvl == 2260 | Wvl == 2200 | Wvl == 1660 |Wvl == 1600 | Wvl == 2330)

CAI$X <- NULL

CAI <- CAI %>%
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000) %>%
  mutate(R2220 = 2200 / 2000)%>%
  mutate(R1620 = 2200 / 2000)%>%
  mutate(RSWIR = 2200 / 2000)%>%
  mutate(ROLI = 2200 / 2000)


CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2200`) - CAI$`2100`)
CAI$SINDRI <- 100 * (CAI$`2200` - CAI$`2260`) / (CAI$`2200` + CAI$`2260`)
CAI$NDTI <- (CAI$`1660` - CAI$`2330`) / (CAI$`1660` + CAI$`2330`)
CAI$R2220 <- CAI$`2200`/CAI$`2000`
CAI$R1620 <- CAI$`1600`/CAI$`2200`
CAI$RSWIR <- CAI$`1660`/CAI$`2260`
CAI$ROLI <- CAI$`1600`/CAI$`2200`

write.csv(CAI, file = paste0(path_to_data, "CAI_Residue_transformed.csv"), row.names = FALSE)



colnames(Soil_Median)[6] <- "Reflectance"
CAI1 <- Soil_Median %>%
  dplyr::filter(Wvl == 2200 | Wvl == 2000 | Wvl == 2100 | Wvl == 2260 | Wvl == 2200 | Wvl == 1660 |Wvl == 1600 | Wvl == 2330)
CAI1$X <- NULL

CAI1 <- CAI1 %>%
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000)%>%
  mutate(R2220 = 2200 / 2000)%>%
  mutate(R1620 = 2200 / 2000)%>%
  mutate(RSWIR = 2200 / 2000)%>%
  mutate(ROLI = 2200 / 2000)

CAI1$CAI <- (0.5 * (CAI1$`2000` + CAI1$`2200`) - CAI1$`2100`)
CAI1$SINDRI <- 100 * (CAI1$`2200` - CAI1$`2260`) / (CAI1$`2200` + CAI1$`2260`)
CAI1$NDTI <- (CAI1$`1660` - CAI1$`2330`) / (CAI1$`1660` + CAI1$`2330`)
CAI1$R2220 <- CAI1$`2200`/CAI1$`2000`
CAI1$R1620 <- CAI1$`1600`/CAI1$`2200`
CAI1$RSWIR <- CAI1$`1660`/CAI1$`2260`
CAI1$ROLI <- CAI1$`1600`/CAI1$`2200`

write.csv(CAI1, file = paste0(path_to_data, "CAI_Soil_transformed.csv"), row.names = FALSE)


CAI <- rbind(CAI1, CAI)
CAI$Scan <- gsub(" ", "", CAI$Scan)

write.csv(CAI, file = paste0(path_to_data, "CAI_transformed_Combined.csv"), row.names = FALSE)


