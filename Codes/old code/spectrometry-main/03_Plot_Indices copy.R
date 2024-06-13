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
setwd("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main")

# Check the current working directory
getwd()

Residue_Median <- read.csv("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main/Residue_08_18.csv")
Residue_Median$Sample <- gsub("Crop Residue", "Residue", Residue_Median$Sample)
Residue_Median <- dplyr::rename(Residue_Median, Type_Name = Crop)

Soil_Median <- read.csv("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/Spectrometry-main/Soil_08_18.csv")

# Residue_Median <- Residue_Median[Residue_Median$Wvl > 1500, ]
# Soil_Median <- Soil_Median[Soil_Median$Wvl > 1500, ]

# Rename Type_Name to Crop
colnames(Residue_Median)[colnames(Residue_Median) == "Type_Name"] <- "Crop"

# Similar to the previous method, you can use colnames() to rename a column
colnames(Soil_Median)[colnames(Soil_Median) == "Type_Name"] <- "Crop"


## Renaming the column to Reflectance
colnames(Residue_Median)[5] <- "Reflectance"

CAI <- Residue_Median %>%
  dplyr::filter(Wvl == 2200 | Wvl == 2000 | Wvl == 2100 | Wvl == 2260 | Wvl == 2205 | Wvl == 1660 |Wvl == 1600 | Wvl == 2330)

CAI <- CAI %>%
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000) %>%
  mutate(R2220 = 2200 / 2000) %>%
  mutate(R1620 = 2200 / 2000) %>%
  mutate(RSWIR = 2200 / 2000) %>%
  mutate(ROLI = 2200 / 2000)


CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2200`) - CAI$`2100`)
CAI$SINDRI <- (CAI$`2205` - CAI$`2260`) / (CAI$`2205` + CAI$`2260`)
CAI$NDTI <- (CAI$`1660` - CAI$`2330`) / (CAI$`1660` + CAI$`2330`)
CAI$R2220 <- CAI$`2200`/CAI$`2000`
CAI$R1620 <- CAI$`1600`/CAI$`2000`
CAI$RSWIR <- CAI$`1660`/CAI$`2260`
CAI$ROLI <- CAI$`1660`/CAI$`2330`

write.csv(CAI, file = "CAI_Residue.csv", row.names = FALSE)


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

colnames(Soil_Median)[5] <- "Reflectance"
CAI1 <- Soil_Median %>%
  dplyr::filter(Wvl == 2200 | Wvl == 2000 | Wvl == 2100 | Wvl == 2260 | Wvl == 2205 | Wvl == 1660 |Wvl == 1600 | Wvl == 2330)

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
CAI1$SINDRI <- (CAI1$`2205` - CAI1$`2260`) / (CAI1$`2205` + CAI1$`2260`)
CAI1$NDTI <- (CAI1$`1660` - CAI1$`2330`) / (CAI1$`1660` + CAI1$`2330`)
CAI1$R2220 <- CAI1$`2200`/CAI1$`2000`
CAI1$R1620 <- CAI1$`1600`/CAI1$`2200`
CAI1$RSWIR <- CAI1$`1660`/CAI1$`2260`
CAI1$ROLI <- CAI1$`1600`/CAI1$`2200`

write.csv(CAI1, file = "CAI_Soil.csv", row.names = FALSE)


CAI <- rbind(CAI1, CAI)
CAI$Scan <- gsub(" ", "", CAI$Scan)

write.csv(CAI, file = "CAI_Combined.csv", row.names = FALSE)


ggplot(CAI, aes(x = RWC, y = CAI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "CAI") +
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
  labs(x = "RWC", y = "SWIR3/SWIR6") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = ROLI, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "OLI6/OLI7") +
  theme(text = element_text(size = 20))+
  coord_flip()

##########################################################
##########################################################
#############  RWC ~ INDEX RATIO (FACET)   ###############
library(ggplot2)
library(patchwork)
library(viridis)

CAI$color_var <- ifelse(CAI$Sample == "Soil", "Soil", CAI$Crop)
color_levels <- unique(CAI$color_var)
CAI$color_var <- factor(CAI$color_var, levels = c("Soil", color_levels[color_levels != "Soil"]))

p3 <- ggplot(CAI, aes(x = RWC, y = R2220, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(CAI$color_var))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.2)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "R2.2/R2.0", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  ) +
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

p4 <- ggplot(CAI, aes(x = RWC, y = R1620, group = Sample, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(CAI$Sample))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.4)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "R1.6/R2.0", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10), legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  ) +
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

p5 <- ggplot(CAI, aes(x = RWC, y = RSWIR, group = Sample, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(CAI$Sample))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "SWIR3/SWIR6", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10), legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  ) +
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

p6 <- ggplot(CAI, aes(x = RWC, y = ROLI, group = Sample, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(CAI$Sample))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "OLI6/OLI7", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10), legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  )+
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

library(patchwork)

combined_plot <- (p3 + p4 + p5 + p6) + plot_layout(ncol=2, guides='collect')
print(combined_plot)

setwd("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/plots/SpectraRatio ~ RWC")


ggsave(filename = "SpectraR ~ RWC (updated data).png", plot = combined_plot, width = 8.3, height = 5.8, dpi = 200)


## Chatgpt's suggestion

library(RColorBrewer)
library(viridis)

n_colors <- length(color_levels)
my_colors <- viridis(n_colors)
my_colors <- viridis_pal(option = "viridis")(n_colors)
scale_color_manual(values = my_colors)


##########################################################
##########################################################
################  RWC ~ INDEX (FACET)   ###############
library(ggplot2)
library(tidyr)
library(viridis)

# Reshape the data into a tidy format
tidy_data <- CAI %>%
  gather(variable, value, SINDRI, CAI, NDTI)

tidy_data$color_group <- ifelse(tidy_data$Sample == "Residue", 
                                paste(tidy_data$Sample, tidy_data$Crop),
                                tidy_data$Sample)
# Define shapes
my_shapes <- c(16:25)  # Adjust based on the number of unique values in color_group
unique_groups <- unique(tidy_data$color_group)
if (length(unique_groups) > length(my_shapes)) {
  stop("You need more shapes!")
}
# color_palette <- c(residue_colors, soil_color)

distinct_shapes <- c(1, 2, 5, 8, 0, 6, 3, 4, 7, 9)  # Add more shapes here as needed
unique_groups <- unique(tidy_data$color_group)
if (length(unique_groups) > length(distinct_shapes)) {
  stop("You need more shapes!")
}

ggplot(tidy_data, aes(x = RWC, y = value, group = Crop)) +
  geom_point(aes(shape = color_group), size = 1.5) +
  geom_smooth(method = 'loess', se = FALSE, size = 0.7, aes(color = color_group)) + # Smooth line
  # geom_line(color = viridis(1)[1], size = 0.4) +  # Set a fixed viridis color for all lines
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "") +
  scale_shape_manual(values = distinct_shapes) +  # Use distinct shapes here
  scale_color_viridis(discrete = TRUE) + # Use viridis color palette
  facet_grid(variable ~ ., scales = "free_y") +
  theme_minimal() + # Apply the minimalistic theme
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),           # Remove major grid lines
    panel.grid.minor = element_blank(),           # Remove minor grid lines
    strip.background = element_rect(fill = "white"), # Remove facet strip background
    axis.text.y = element_text(size = 12), 
    text = element_text(size = 12)
  ) +
  labs(shape = "Sample", color = "Sample")

my_plot <- ggplot(tidy_data, aes(x = RWC, y = value, group = Crop)) +
  geom_point(aes(shape = color_group), size = 1.5) +
  geom_smooth(method = 'loess', se = FALSE, size = 0.4, aes(color = color_group)) + # Smooth line
  # geom_line(color = viridis(1)[1], size = 0.4) +  # Set a fixed viridis color for all lines
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "") +
  scale_shape_manual(values = distinct_shapes) +  # Use distinct shapes here
  scale_color_viridis(discrete = TRUE) + # Use viridis color palette
  facet_grid(variable ~ ., scales = "free_y") +
  theme_minimal() + # Apply the minimalistic theme
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),           # Remove major grid lines
    panel.grid.minor = element_blank(),           # Remove minor grid lines
    strip.background = element_rect(fill = "white"), # Remove facet strip background
    axis.text.y = element_text(size = 12), 
    text = element_text(size = 12)
  ) +
  labs(shape = "Sample", color = "Sample")

setwd("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/plots/Index ~ RWC")


ggsave(filename = "Index ~ RWC (Updated data).png", plot = my_plot, width = 8.3, height = 5.8, dpi = 200)


##########################################################
##########################################################
#############  RWC ~ Reflectance (FACET)   ###############
library(ggplot2)
library(patchwork)

CAI$color_var <- ifelse(CAI$Sample == "Soil", "Soil", CAI$Crop)
color_levels <- unique(CAI$color_var)
CAI$color_var <- factor(CAI$color_var, levels = c("Soil", color_levels[color_levels != "Soil"]))

setwd("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/plots/RWC ~ Ref")
write.csv(CAI, file = "CAI_test.csv", row.names = FALSE)

df_long <- CAI %>%
  pivot_longer(
    cols = c(`1600`, `1660`,`2000` , `2200`, `2330`, `2260`), names_to = "Wvl", values_to = "Reflect")

df_long <- df_long %>%
  mutate(Wvl = case_when(
    Wvl == 2200 ~ "R2.2",
    Wvl == 2000 ~ "R2.0",
    Wvl == 1600 ~ "R1.6",
    Wvl == 1660 ~ "SWIR3",
    Wvl == 2260 ~ "SWIR6",
    Wvl == 1660 ~ "OLI6",
    Wvl == 2330 ~ "OLI7",
    TRUE ~ "Other"
  ))

df_long$shape_value <- paste(df_long$Crop, as.character(df_long$Wvl))

df_Soil <- df_long[df_long$Sample %in% "Soil", ]
df_Res <- df_long[df_long$Sample %in% "Residue", ]

for (soil in unique(df_Soil$Crop)) {
  for (crp in unique(df_Res$Crop)){
    # Filter dataframe based on multiple string values in the Gender column
    values_to_include <- c(soil, crp)  # Values to filter for
    df <- df_long[df_long$Crop %in% values_to_include, ]
    
    # List to hold data frames with predictions for each category
    list_of_dfs <- list()
    
    # Loop through each unique category in `shape_value`
    for (category in unique(df$shape_value)) {
      sub_df <- subset(df, shape_value == category)
      loess_fit <- loess(Reflect ~ RWC, data = sub_df)
      predictions <- predict(loess_fit)
      residuals <- sub_df$Reflect - predictions
      sub_df <- mutate(sub_df, residuals = residuals, predictions = predictions)
      
      # Filter the data by residual, for example, within +/- 0.1 of the prediction
      sub_df <- filter(sub_df, abs(residuals) <= 2)
      
      list_of_dfs[[category]] <- sub_df
    }
    
    # Combine the filtered data back into one data frame
    df_new <- bind_rows(list_of_dfs)
    
    R_values_to_include <- c("R1.6", "R2.0", "R2.2")  # Values to filter for
    df_R216 <- df_new[df_new$Wvl %in% R_values_to_include, ]
    
    
    p1 <- ggplot(df_R216, aes(x = RWC, y = Reflect, shape = shape_value)) + 
      geom_point(size=2) +
      geom_smooth(aes(color = Sample), method = "loess", se=FALSE, span = TRUE,
                  fullrange = TRUE, size = 0.4) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(breaks = seq(0, 100, by = 5)) +
      labs(x = "RWC", y = "Reflectance", color = "Sample", shape = "Sample") +
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),           # Remove major grid lines
            panel.grid.minor = element_blank(),           # Remove minor grid lines
            axis.line = element_line(color = "black"),
            strip.background = element_rect(fill = "white") # Remove facet strip background
      )
    p1 <- p1 + guides(color = FALSE)
    p1 <- p1 + scale_color_manual(values = c("Soil" = "black", "Residue" = "orange"))
    
    print(p1)
    
    R_values_to_include <- c("SWIR3", "SWIR6")  # Values to filter for
    df_SWIR <- df_new[df_new$Wvl %in% R_values_to_include, ]
    
    
    p2 <- ggplot(df_SWIR, aes(x = RWC, y = Reflect, shape = shape_value)) + 
      geom_point(size=2) +
      geom_smooth(aes(color = Sample), method = "loess", se=FALSE, span = TRUE,
                  fullrange = TRUE, size = 0.4) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(breaks = seq(0, 100, by = 5)) +
      labs(x = "RWC", y = "Reflectance", color = "Sample", shape = "Sample") +
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),           # Remove major grid lines
            panel.grid.minor = element_blank(),           # Remove minor grid lines
            axis.line = element_line(color = "black"),
            strip.background = element_rect(fill = "white") # Remove facet strip background
      )
    p2 <- p2 + guides(color = FALSE)
    p2 <- p2 + scale_color_manual(values = c("Soil" = "black", "Residue" = "orange"))
    
    print(p2)
    
    
    df_forOLI <- df_new %>%
      mutate(Wvl = case_when(
        Wvl == "R2.2" ~ "R2.2",
        Wvl == "R2.0" ~ "R2.0",
        Wvl == "R1.6" ~ "R1.6",
        Wvl == "SWIR3" ~ "OLI6",
        Wvl == "SWIR6" ~ "SWIR6",
        Wvl == "OLI7" ~ "OLI7",
        TRUE ~ "Other"
      ))
    
    df_forOLI$shape_value <- paste(df_forOLI$Crop, as.character(df_forOLI$Wvl))
    
    R_values_to_include <- c("OLI6", "OLI7")  # Values to filter for
    df_OLI <- df_forOLI[df_forOLI$Wvl %in% R_values_to_include, ]
    
    
    p3 <- ggplot(df_OLI, aes(x = RWC, y = Reflect, shape = shape_value)) + 
      geom_point(size=2) +
      geom_smooth(aes(color = Sample), method = "loess", se=FALSE, span = TRUE,
                  fullrange = TRUE, size = 0.4) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(breaks = seq(0, 100, by = 5)) +
      labs(x = "RWC", y = "Reflectance", color = "Sample", shape = "Sample") +
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),           # Remove major grid lines
            panel.grid.minor = element_blank(),           # Remove minor grid lines
            axis.line = element_line(color = "black"),
            strip.background = element_rect(fill = "white") # Remove facet strip background
      )
    p3 <- p3 + guides(color = FALSE)
    p3 <- p3 + scale_color_manual(values = c("Soil" = "black", "Residue" = "orange"))
    
    print(p3)
    
    library(patchwork)
    
    p1 <- p1 + theme(legend.key.size = unit(0.6, "cm"))
    p2 <- p2 + theme(legend.key.size = unit(0.6, "cm"))
    p3 <- p3 + theme(legend.key.size = unit(0.6, "cm"))
    
    combined_plot <- (p1 + p2 + p3) + plot_layout(ncol=1)
    print(combined_plot)
    
    setwd("/Users/aminnorouzi/Library/CloudStorage/GoogleDrive-msaminnorouzi@gmail.com/My Drive/PhD/Projects/Spectroscopy paper/EPO/plots/RWC ~ Ref/Updated_data")
    
    
    ggsave(filename = paste(soil,"_", crp, ".png"), plot = combined_plot, width = 5, height = 8, dpi = 300)
  }
}

