library(reshape2)
library(dplyr)
library(ggplot2)

##################################################################
##################################################################
##################################################################

########################### AFTER EPO ###########################

##################################################################
##################################################################
##################################################################
path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_save <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/',
                       'index_org_trsfed_crp_sl/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# 
# path_to_save <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/',
#                        'index_org_trsfed_crp_sl/')

CAI <- read.csv(paste0(path_to_data, "CAI_transformed_Combined.csv"))

# desired_colOrder <- c("Sample", "Scan", "Type", "RWC", "X1600", "X1660", "X2000", "X2100", "X2200",
# "X2260", "X2330", "CAI", "SINDRI", "NDTI", "R2220", "R1620", "RSWIR", "ROLI")
# # Reorder the columns
# CAI <- CAI[, desired_colOrder]

crops = unique(CAI[CAI$Sample == "Residue", ]$Type)
soils = unique(CAI[CAI$Sample == "Soil", ]$Type)

fraction_list <- list()

crp <- crops[1]
sl <- soils[1]

for (crp in sort(crops)) {
  for (sl in sort(soils)) {
    
    fractions <- runif(20, min = 0, max = 1)
    fraction_name = paste0('fraction_', crp,' ', sl)
    fraction_list[[fraction_name]] <- fractions
    
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
    
    
    for (i in length(unique(CAI$Scan))) {
      a <- dplyr::filter(CAI, Sample == "Soil")
      b <- dplyr::filter(CAI, Sample == "Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "Scan", by.y = "Scan")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
      }
    
    test1_NDTI <- f
    
    for (i in fractions){
      colname <-  paste0("CAI_", i)
      test1_NDTI[[colname]] <- test1_NDTI$NDTI.x * (1 - i) + test1_NDTI$NDTI.y * i
    }
    
    test2_NDTI <- dplyr::filter(test1_NDTI, Type.x == sl & Type.y == crp)
    
    test3_NDTI <- test2_NDTI[c(24,42:length(test2_NDTI))] ## select the conven,med,conser colums
    
    test3_NDTI <- reshape2::melt(test3_NDTI, id = "RWC.y")
    
    
    for (i in fractions){
      pattern <- paste0("CAI_", i)
      test3_NDTI$variable <- gsub(pattern, i, test3_NDTI$variable)
      # test3_NDTI$variable <- as.numeric(test3_NDTI$variable)
    }
    
    names(test3_NDTI)[2] <- "Fraction_Residue_Cover"
    names(test3_NDTI)[3] <- "CAI"
    
    write.csv(test3_NDTI, paste0(path_to_save, "NDTI_transformed_", crp, "_", gsub("_", " ", sl), ".csv"), row.names = FALSE)
    
    ##Fig5
    ggplot(test3_NDTI, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
      geom_line(aes(color = factor(RWC.y))) +
      geom_point(aes(shape = factor(RWC.y)))+
      labs(y = "Fraction Residue Cover", x = "NDTI") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
            legend.title=element_blank(),
            legend.margin=margin(c(1,5,5,5)))
    
    ##################################################################
    ##################################################################
    ##################################################################
    
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
    
    
    for (i in length(unique(CAI$Scan))) {
      a <- dplyr::filter(CAI, Sample == "Soil")
      b <- dplyr::filter(CAI, Sample == "Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "Scan", by.y = "Scan")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_CAI <- f
    
    for (i in fractions){
      colname <-  paste0("CAI_", i)
      test1_CAI[[colname]] <- test1_CAI$CAI.x * (1 - i) + test1_CAI$CAI.y * i
    }
  
    test2_CAI <- dplyr::filter(test1_CAI, Type.x == sl & Type.y == crp)
    
    test3_CAI <- test2_CAI[c(24,42:length(test2_NDTI))] ## select the conven,med,conser colums
    
    test3_CAI <- reshape2::melt(test3_CAI, id = "RWC.y")
    
    
    for (i in fractions){
      pattern <- paste0("CAI_", i)
      test3_CAI$variable <- gsub(pattern, i, test3_CAI$variable)
      # test3_CAI$variable <- as.numeric(test3_CAI$variable)
    }
    
    names(test3_CAI)[2] <- "Fraction_Residue_Cover"
    names(test3_CAI)[3] <- "CAI"
    
    write.csv(test3_CAI, paste0(path_to_save, "CAI_transformed_", crp, "_", gsub("_", " ", sl), ".csv"), row.names = FALSE)
    
    ##Fig5
    ggplot(test3_CAI, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
      geom_line(aes(color = factor(RWC.y))) +
      geom_point(aes(shape = factor(RWC.y)))+
      labs(y = "Fraction Residue Cover", x = "CAI") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
            legend.title=element_blank(),
            legend.margin=margin(c(1,5,5,5)))
    
    ##################################################################
    ##################################################################
    ##################################################################
    
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
    
    
    for (i in length(unique(CAI$Scan))) {
      a <- dplyr::filter(CAI, Sample == "Soil")
      b <- dplyr::filter(CAI, Sample == "Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "Scan", by.y = "Scan")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_SINDRI <- f
    
    for (i in fractions){
      colname <-  paste0("CAI_", i)
      test1_SINDRI[[colname]] <- test1_SINDRI$SINDRI.x * (1 - i) + test1_SINDRI$SINDRI.y * i
    }
    
    test2_SINDRI <- dplyr::filter(test1_SINDRI, Type.x == sl & Type.y == crp)
  
    test3_SINDRI <- test2_SINDRI[c(24,42:length(test2_NDTI))] ## select the conven,med,conser colums
    
    test3_SINDRI <- reshape2::melt(test3_SINDRI, id = "RWC.y")
    
    for (i in fractions){
      pattern <- paste0("CAI_", i)
      test3_SINDRI$variable <- gsub(pattern, i, test3_SINDRI$variable)
      # test3_SINDRI$variable <- as.numeric(test3_SINDRI$variable)
    }
    
    names(test3_SINDRI)[2] <- "Fraction_Residue_Cover"
    names(test3_SINDRI)[3] <- "CAI"
    
    write.csv(test3_SINDRI, paste0(path_to_save, "SINDRI_transformed_", crp, "_", gsub("_", " ", sl), ".csv"), row.names = FALSE)
    
    ##Fig5
    ggplot(test3_SINDRI, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
      geom_line(aes(color = factor(RWC.y))) +
      geom_point(aes(shape = factor(RWC.y)))+
      labs(y = "Fraction Residue Cover", x = "SINDRI") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
            legend.title=element_blank(),
            legend.margin=margin(c(1,5,5,5)))
    
  }
}
  ##################################################################
  ##################################################################
  ##################################################################

  ########################### BEFORE EPO ###########################

  ##################################################################
  ##################################################################
  ##################################################################


CAI <- read.csv(paste0(path_to_data, "CAI_Combined.csv"))



# desired_colOrder <- c("Sample", "Scan", "Type", "RWC", "X1600", "X1660", "X2000", "X2100", "X2200",
#                       "X2260", "X2330", "CAI", "SINDRI", "NDTI", "R2220", "R1620", "RSWIR", "ROLI")
# # Reorder the columns
# CAI <- CAI[, desired_colOrder]

for (crp in sort(crops)) {
  for (sl in sort(soils)) {
    
    fractions <- fraction_list[[paste0('fraction_', crp,' ', sl)]]
      
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
  
  
    for (i in length(unique(CAI$Scan))) {
      a <- dplyr::filter(CAI, Sample == "Soil")
      b <- dplyr::filter(CAI, Sample == "Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "Scan", by.y = "Scan")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_NDTI <- f
    
    for (i in fractions){
      colname <-  paste0("CAI_", i)
      test1_NDTI[[colname]] <- test1_NDTI$NDTI.x * (1 - i) + test1_NDTI$NDTI.y * i
    }
  
    test2_NDTI <- dplyr::filter(test1_NDTI, Type.x == sl & Type.y == crp)
  
    test3_NDTI <- test2_NDTI[c(24,43:length(test2_NDTI))] ## select the conven,med,conser colums
  
    test3_NDTI <- reshape2::melt(test3_NDTI, id = "RWC.y")
  
    for (i in fractions){
      pattern <- paste0("CAI_", i)
      test3_NDTI$variable <- gsub(pattern, i, test3_NDTI$variable)
      # test3_NDTI$variable <- as.numeric(test3_NDTI$variable)
    }
    names(test3_NDTI)[2] <- "Fraction_Residue_Cover"
    names(test3_NDTI)[3] <- "CAI"
  
    write.csv(test3_NDTI, paste0(path_to_save, "NDTI_Original_", crp, "_", gsub("_", " ", sl), ".csv"), row.names = FALSE)
  
    ##Fig5
    ggplot(test3_NDTI, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
      geom_line(aes(color = factor(RWC.y))) +
      geom_point(aes(shape = factor(RWC.y)))+
      labs(y = "Fraction Residue Cover", x = "NDTI") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
            legend.title=element_blank(),
            legend.margin=margin(c(1,5,5,5)))
  
    ##################################################################
    ##################################################################
    ##################################################################
  
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
  
  
    for (i in length(unique(CAI$Scan))) {
      a <- dplyr::filter(CAI, Sample == "Soil")
      b <- dplyr::filter(CAI, Sample == "Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "Scan", by.y = "Scan")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_CAI <- f
  
    for (i in fractions){
      colname <-  paste0("CAI_", i)
      test1_CAI[[colname]] <- test1_CAI$CAI.x * (1 - i) + test1_CAI$CAI.y * i
    }
    
    
    test2_CAI <- dplyr::filter(test1_CAI, Type.x == sl & Type.y == crp)
  
    test3_CAI <- test2_CAI[c(24,42:length(test2_NDTI))]
  
    test3_CAI <- reshape2::melt(test3_CAI, id = "RWC.y")
  
    for (i in fractions){
      pattern <- paste0("CAI_", i)
      test3_CAI$variable <- gsub(pattern, i, test3_CAI$variable)
      # test3_CAI$variable <- as.numeric(test3_CAI$variable)
    }
    
    names(test3_CAI)[2] <- "Fraction_Residue_Cover"
    names(test3_CAI)[3] <- "CAI"
  
    write.csv(test3_CAI, paste0(path_to_save, "CAI_Original_" , crp, "_", gsub("_", " ", sl), ".csv"), row.names = FALSE)
  
    ##Fig5
    ggplot(test3_CAI, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
      geom_line(aes(color = factor(RWC.y))) +
      geom_point(aes(shape = factor(RWC.y)))+
      labs(y = "Fraction Residue Cover", x = "CAI") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
            legend.title=element_blank(),
            legend.margin=margin(c(1,5,5,5)))
  
    ##################################################################
    ##################################################################
    ##################################################################
  
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
  
  
    for (i in length(unique(CAI$Scan))) {
      a <- dplyr::filter(CAI, Sample == "Soil")
      b <- dplyr::filter(CAI, Sample == "Residue")
      for (j in unique(a$Type)) {
        c <- dplyr::filter(a, Type == j)
        d <- merge(c, b, by.x = "Scan", by.y = "Scan")
        e <- rbind(e, d)
      }
      f <- rbind(f, e)
    }
    
    test1_SINDRI <- f
  
    
    for (i in fractions){
      colname <-  paste0("CAI_", i)
      test1_SINDRI[[colname]] <- test1_SINDRI$SINDRI.x * (1 - i) + test1_SINDRI$SINDRI.y * i
    }
    
    test2_SINDRI <- dplyr::filter(test1_SINDRI, Type.x == sl & Type.y == crp)
  
    test3_SINDRI <- test2_SINDRI[c(24,42:length(test2_NDTI))] ## select the conven,med,conser colums
  
    test3_SINDRI <- reshape2::melt(test3_SINDRI, id = "RWC.y")
  
    for (i in fractions){
      pattern <- paste0("CAI_", i)
      test3_SINDRI$variable <- gsub(pattern, i, test3_SINDRI$variable)
      # test3_SINDRI$variable <- as.numeric(test3_SINDRI$variable)
    }
    
    names(test3_SINDRI)[2] <- "Fraction_Residue_Cover"
    names(test3_SINDRI)[3] <- "CAI"
  
    write.csv(test3_SINDRI, paste0(path_to_save, "SINDRI_Original_", crp, "_", gsub("_", " ", sl),  ".csv"), row.names = FALSE)
  
    ##Fig5
    ggplot(test3_SINDRI, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.y))) +
      geom_line(aes(color = factor(RWC.y))) +
      geom_point(aes(shape = factor(RWC.y)))+
      labs(y = "Fraction Residue Cover", x = "SINDRI") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
            legend.title=element_blank(),
            legend.margin=margin(c(1,5,5,5)))
  }
}
  
  
  
  

