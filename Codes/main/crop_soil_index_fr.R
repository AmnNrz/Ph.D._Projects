library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')



index_comb <- read.csv(paste0(path_to_data, "index_combined.csv"))

crops = unique(index_comb[index_comb$Sample == "Crop Residue", ]$Type)
soils = unique(index_comb[index_comb$Sample == "Soil", ]$Type)

fraction_list <- list()

crp <- crops[1]
sl <- soils[1]

for (crp in sort(crops)) {
  for (sl in sort(soils)) {

    # fractions <- runif(10, min = 0, max = 1)
    fractions <- seq(from = 0, to = 1, by = 0.1)
      
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
  
  
    for (i in length(unique(index_comb$RWC))) {
      a <- dplyr::filter(index_comb, Sample == "Soil")
      b <- dplyr::filter(index_comb, Sample == "Crop Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "RWC", by.y = "RWC")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_NDTI <- f
    
    for (i in fractions){
      colname <-  paste0("index_comb_", i)
      test1_NDTI[[colname]] <- test1_NDTI$NDTI.x * (1 - i) + test1_NDTI$NDTI.y * i
    }
  
    test2_NDTI <- dplyr::filter(test1_NDTI, Type.x == sl & Type.y == crp)
    
    # Select residue RWC (1) column and index columns (40:)
    test3_NDTI <- test2_NDTI[c(1,40:length(test2_NDTI))]
  
    test3_NDTI <- reshape2::melt(test3_NDTI, id = "RWC")
  
    for (i in fractions){
      pattern <- paste0("index_comb_", i)
      test3_NDTI$variable <- gsub(pattern, i, test3_NDTI$variable)
      # test3_NDTI$variable <- as.numeric(test3_NDTI$variable)
    }
    names(test3_NDTI)[2] <- "Fraction_Residue_Cover"
    names(test3_NDTI)[3] <- "index"
  
    write.csv(test3_NDTI, paste0(path_to_data, "crp_sl_index_fr/NDTI_Original_", crp, "_", gsub("_", " ", sl), ".csv"), row.names = FALSE)
  
    ##################################################################
    ##################################################################
    ##################################################################
  
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
  
  
    for (i in length(unique(index_comb$RWC))) {
      a <- dplyr::filter(index_comb, Sample == "Soil")
      b <- dplyr::filter(index_comb, Sample == "Crop Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "RWC", by.y = "RWC")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_CAI <- f
  
    for (i in fractions){
      colname <-  paste0("index_comb_", i)
      test1_CAI[[colname]] <- test1_CAI$CAI.x * (1 - i) + test1_CAI$CAI.y * i
    }
    
    
    test2_CAI <- dplyr::filter(test1_CAI, Type.x == sl & Type.y == crp)
  
    test3_CAI <- test2_CAI[c(1,40:length(test2_CAI))]
  
    test3_CAI <- reshape2::melt(test3_CAI, id = "RWC")
  
    for (i in fractions){
      pattern <- paste0("index_comb_", i)
      test3_CAI$variable <- gsub(pattern, i, test3_CAI$variable)
      # test3_index_comb$variable <- as.numeric(test3_index_comb$variable)
    }
    
    names(test3_CAI)[2] <- "Fraction_Residue_Cover"
    names(test3_CAI)[3] <- "index"
  
    write.csv(test3_CAI, paste0(path_to_data, "crp_sl_index_fr/CAI_Original_" , crp, "_", gsub("_", " ", sl), ".csv"), row.names = FALSE)
  
    ##################################################################
    ##################################################################
    ##################################################################
  
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
  
  
    for (i in length(unique(index_comb$RWC))) {
      a <- dplyr::filter(index_comb, Sample == "Soil")
      b <- dplyr::filter(index_comb, Sample == "Crop Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "RWC", by.y = "RWC")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_SINDRI <- f
  
    
    for (i in fractions){
      colname <-  paste0("index_comb_", i)
      test1_SINDRI[[colname]] <- test1_SINDRI$SINDRI.x * (1 - i) + test1_SINDRI$SINDRI.y * i
    }
    
    test2_SINDRI <- dplyr::filter(test1_SINDRI, Type.x == sl & Type.y == crp)
  
    test3_SINDRI <- test2_SINDRI[c(1,40:length(test2_SINDRI))] ## select the conven,med,conser colums
  
    test3_SINDRI <- reshape2::melt(test3_SINDRI, id = "RWC")
  
    for (i in fractions){
      pattern <- paste0("index_comb_", i)
      test3_SINDRI$variable <- gsub(pattern, i, test3_SINDRI$variable)
      # test3_SINDRI$variable <- as.numeric(test3_SINDRI$variable)
    }
    
    names(test3_SINDRI)[2] <- "Fraction_Residue_Cover"
    names(test3_SINDRI)[3] <- "index"
  
    write.csv(test3_SINDRI, paste0(path_to_data, "crp_sl_index_fr/SINDRI_Original_", crp, "_", gsub("_", " ", sl),  ".csv"), row.names = FALSE)
  }
}

