library(tidyverse)
library(dplyr)
library(ggplot2)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/')

Residue_Median <- read.csv(paste0(path_to_data, 
                                  "Residue_Transformed.csv"),
                           header = TRUE, row.names = NULL)
# Residue_Median <- Residue_Median[-c(1, 7)]

Soil_Median <- read.csv(paste0(path_to_data,
                               "Soil_Transformed.csv"),
                        header = TRUE, row.names = NULL)
# Soil_Median <- Soil_Median[-c(1, 7)]

# Residue_Median <- rename(Residue_Median, Crop = Type_Name)
# Soil_Median <- rename(Soil_Median, Crop = Type_Name)

# Residue_Median <- Residue_Median %>%
#   mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))

Residue_Median <- Residue_Median[Residue_Median$Wvl > 1400, ]
Soil_Median <- Soil_Median[Soil_Median$Wvl > 1400, ]



                    
## Renaming the column to Reflectance
colnames(Residue_Median)[4] <- "Reflectance"

select_columns_range <- function(df, start_col_name, end_col_name) {
  start_col <- which(names(df) == start_col_name)
  end_col <- which(names(df) == end_col_name)
  if (start_col == 0 || end_col == 0) {
    stop("One of the specified column names does not exist in the dataframe.")
  }
  selected_df <- df[, start_col:end_col]
  return(selected_df)
}
CAI <- Residue_Median %>%
  dplyr::filter(Wvl >= 1660 | Wvl <= 2280)

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



CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2250`) - CAI$`2090`)
# CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2200`) - CAI$`2100`)


CAI$R2220_2260 <-  rowMeans(select_columns_range(CAI, '2220', '2260'))
CAI$R2260_2280 <-  rowMeans(select_columns_range(CAI, '2295', '2330'))
# CAI$SINDRI <- (CAI$R2220_2260 - CAI$R2260_2280) / (CAI$R2220_2260 + CAI$R2260_2280)
CAI$SINDRI <- (CAI$`2200` - CAI$`2260`) / (CAI$`2200` + CAI$`2260`)

CAI$R1660_1690 <-  rowMeans(select_columns_range(CAI, '1660', '1690'))
CAI$R2220_2280 <-  rowMeans(select_columns_range(CAI, '2220', '2280'))
CAI$NDTI <- (CAI$R1660_1690 - CAI$R2220_2280) / (CAI$R1660_1690 + CAI$R2220_2280)
# CAI$NDTI <- (CAI$`1660` - CAI$`2330`) / (CAI$`1660` + CAI$`2330`)

CAI$R2220 <- CAI$`2250`/CAI$`2000`
CAI$R1620 <- CAI$`1600`/CAI$`2000`
CAI$RSWIR <- CAI$`1660`/CAI$`R2260_2280`
CAI$ROLI <- CAI$`1660`/CAI$R2220_2280

desired_column <- c("Sample", "Crop", "Scan", "RWC", "CAI", "SINDRI", "NDTI", "R2220", "R1620", "RSWIR", "ROLI",
                    "R2220_2260", "R2260_2280", "R1660_1690", "R2220_2280", "2160", "2190", "2180", "2000", "2250", "2090")
CAI <- CAI[, desired_column]

write.csv(CAI, file = paste0(path_to_data, "CAI_Residue_transformed.csv"), row.names = FALSE)


ggplot(CAI, aes(x = RWC, y = CAI, group = Crop, color = Crop)) +
  geom_point() +
  geom_line()
facet_wrap(~Crop, ncol = 2)

ggplot(CAI, aes(x = RWC, y = SINDRI, group = Crop, color = Crop)) +
  geom_point() +
  geom_line()
facet_wrap(~Crop, ncol = 2)

ggplot(CAI, aes(x = RWC, y = NDTI, group = Crop, color = Crop)) +
  geom_point() +
  geom_line()
facet_wrap(~Crop, ncol = 2)

colnames(Soil_Median)[4] <- "Reflectance"

CAI1 <- Soil_Median %>%
  dplyr::filter(Wvl >= 1660 | Wvl <= 2280)
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

CAI1$CAI <- (0.5 * (CAI1$`2160` + CAI1$`2190`) - CAI1$`2180`)
# CAI1$CAI1 <- (0.5 * (CAI1$`2000` + CAI1$`2200`) - CAI1$`2100`)

CAI1$R2220_2260 <-  rowMeans(select_columns_range(CAI1, '2240', '2260'))
CAI1$R2260_2280 <-  rowMeans(select_columns_range(CAI1, '2290', '2330'))
# CAI1$SINDRI <- (CAI1$R2220_2260 - CAI1$R2260_2280) / (CAI1$R2220_2260 + CAI1$R2260_2280)
CAI1$SINDRI <- (CAI1$`2200` - CAI1$`2260`) / (CAI1$`2200` + CAI1$`2260`)

CAI1$R1660_1690 <-  rowMeans(select_columns_range(CAI1, '1660', '1690'))
CAI1$R2220_2280 <-  rowMeans(select_columns_range(CAI1, '2220', '2280'))
CAI1$NDTI <- (CAI1$R1660_1690 - CAI1$R2220_2280) / (CAI1$R1660_1690 + CAI1$R2220_2280)
# CAI1$NDTI <- (CAI1$`1660` - CAI1$`2330`) / (CAI1$`1660` + CAI1$`2330`)

CAI1$R2220 <- CAI1$`2250`/CAI1$`2000`
CAI1$R1620 <- CAI1$`1600`/CAI1$`2000`
CAI1$RSWIR <- CAI1$`1660`/CAI1$`R2260_2280`
CAI1$ROLI <- CAI1$`1660`/CAI1$R2220_2280

CAI1 <- CAI1[, desired_column]



write.csv(CAI1, file = paste0(path_to_data, "CAI_Soil_transformed.csv"), row.names = FALSE)


CAI <- rbind(CAI1, CAI)
CAI$Scan <- gsub(" ", "", CAI$Scan)

write.csv(CAI, file = paste0(path_to_data, "CAI_transformed_Combined.csv"), row.names = FALSE)


ggplot(CAI, aes(x = RWC, y = CAI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "CAI") +
  theme(text = element_text(size = 20))

ggplot(CAI, aes(x = RWC, y = NDTI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "NDTI") +
  theme(text = element_text(size = 20))

ggplot(CAI, aes(x = RWC, y = SINDRI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "SINDRI") +
  theme(text = element_text(size = 20))

scale_x_discrete(limits = rev(levels(as.factor(CAI$Scan))), guide = guide_axis(angle = 90))

ggplot(CAI, aes(x = RWC, y = R2220, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "R2.2/R2.0") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = R2220, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.2)) +
  labs(x = "RWC", y = "R2.2/R2.0") +
  theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
        legend.title=element_blank(),
        legend.margin=margin(c(1,5,5,5)))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = R1620, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "R1.6/R2.0") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = RSWIR, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "SWIR6/SWIR7") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = ROLI, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "OLI6/OLI7") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(dplyr::filter(CAI, Crop == "Wheat Duet" | Crop == "Pomeroy_top"), aes(x = RWC, y = SINDRI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "SINDRI") +
  theme(axis.text.y = element_blank(), text = element_text(size = 20))

ggplot(dplyr::filter(CAI, Crop == "Wheat Duet" | Crop == "Pomeroy_top"), aes(x = RWC, y = CAI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "CAI") +
  theme(axis.text.y = element_blank(), text = element_text(size = 20))

ggplot(dplyr::filter(CAI, Crop == "Wheat Duet" | Crop == "Pomeroy_top"), aes(x = RWC, y = NDTI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "NDTI") +
  theme(axis.text.y = element_blank(), text = element_text(size = 20))

