library(tidyverse)
library(dplyr)
library(ggplot2)


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

Residue_Median <- read.csv(paste0(path_to_data, 
                                  "Residue.csv"),
                           header = TRUE, row.names = NULL)
Residue_Median <- Residue_Median[-c(1, 8)]

Soil_Median <- read.csv(paste0(path_to_data, 
                               "Soil.csv"),
                        header = TRUE, row.names = NULL)
Soil_Median <- Soil_Median[-c(1, 8)]

Residue_Median <- Residue_Median %>%
  rename(Type = Soil)

Soil_Median <- rename(Soil_Median, Type = Soil)

Residue_Median <- Residue_Median %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))

Residue_Median <- Residue_Median[Residue_Median$Wvl > 1400, ]
Soil_Median <- Soil_Median[Soil_Median$Wvl > 1400, ]



## Renaming the column to Reflectance
colnames(Residue_Median)[6] <- "Reflectance"

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
  dplyr::filter(Wvl >= 1660 | Wvl <= 2330)



CAI <- CAI %>%
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000) %>%
  mutate(R2220 = 2200 / 2000) %>%
  mutate(R1620 = 2200 / 2000) %>%
  mutate(RSWIR = 2200 / 2000) %>%
  mutate(ROLI = 2200 / 2000)


# CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2200`) - CAI$`2100`)
CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2250`) - CAI$`2090`)
# CAI$SINDRI <- (CAI$`2200` - CAI$`2260`) / (CAI$`2200` + CAI$`2260`)
CAI$R2220_2260 <-  rowMeans(select_columns_range(CAI, '2240', '2260'))
CAI$R2260_2280 <-  rowMeans(select_columns_range(CAI, '2295', '2330'))
CAI$SINDRI <- (CAI$R2220_2260 - CAI$R2260_2280) / (CAI$R2220_2260 + CAI$R2260_2280)
# CAI$NDTI <- (CAI$`1660` - CAI$`2330`) / (CAI$`1660` + CAI$`2330`)
CAI$R1660_1690 <-  rowMeans(select_columns_range(CAI, '1660', '1690'))
CAI$R2220_2280 <-  rowMeans(select_columns_range(CAI, '2220', '2280'))
CAI$NDTI <- (CAI$R1660_1690 - CAI$R2220_2280) / (CAI$R1660_1690 + CAI$R2220_2280)
CAI$R2220 <- CAI$`2250`/CAI$`2000`
CAI$R1620 <- CAI$`1600`/CAI$`2000`
CAI$RSWIR <- CAI$`1660`/CAI$`R2260_2280`
CAI$ROLI <- CAI$`1660`/CAI$R2220_2280

desired_column <- c("Sample", "Type", "Scan", "RWC", "CAI", "SINDRI", "NDTI", "R2220", "R1620", "RSWIR", "ROLI",
                    "R2220_2260", "R2260_2280", "R1660_1690", "R2220_2280", "2160", "2190", "2180", "2000", "2250", "2090")
CAI <- CAI[, desired_column]

# write.csv(CAI, file = paste0(path_to_data, "CAI_Residue.csv"), row.names = FALSE)


ggplot(CAI, aes(x = RWC, y = CAI, group = Type, color = Type)) +
  geom_point() +
  geom_line()
facet_wrap(~Type, ncol = 2)

ggplot(CAI, aes(x = RWC, y = SINDRI, group = Type, color = Type)) +
  geom_point() +
  geom_line()
facet_wrap(~Type, ncol = 2)

ggplot(CAI, aes(x = RWC, y = NDTI, group = Type, color = Type)) +
  geom_point() +
  geom_line()
facet_wrap(~Type, ncol = 2)

colnames(Soil_Median)[6] <- "Reflectance"
CAI1 <- Soil_Median %>%
  dplyr::filter(Wvl >= 1660 | Wvl <= 2330)

CAI1 <- CAI1 %>%
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000)%>%
  mutate(R2220 = 2200 / 2000)%>%
  mutate(R1620 = 2200 / 2000)%>%
  mutate(RSWIR = 2200 / 2000)%>%
  mutate(ROLI = 2200 / 2000)

# CAI1$CAI1 <- (0.5 * (CAI1$`2000` + CAI1$`2200`) - CAI1$`2100`)
CAI1$CAI <- (0.5 * (CAI1$`2160` + CAI1$`2190`) - CAI1$`2180`)
# CAI1$SINDRI <- (CAI1$`2200` - CAI1$`2260`) / (CAI1$`2200` + CAI1$`2260`)
CAI1$R2220_2260 <-  rowMeans(select_columns_range(CAI1, '2240', '2260'))
CAI1$R2260_2280 <-  rowMeans(select_columns_range(CAI1, '2290', '2330'))
CAI1$SINDRI <- (CAI1$R2220_2260 - CAI1$R2260_2280) / (CAI1$R2220_2260 + CAI1$R2260_2280)
# CAI1$NDTI <- (CAI1$`1660` - CAI1$`2330`) / (CAI1$`1660` + CAI1$`2330`)
CAI1$R1660_1690 <-  rowMeans(select_columns_range(CAI1, '1660', '1690'))
CAI1$R2220_2280 <-  rowMeans(select_columns_range(CAI1, '2220', '2280'))
CAI1$NDTI <- (CAI1$R1660_1690 - CAI1$R2220_2280) / (CAI1$R1660_1690 + CAI1$R2220_2280)
CAI1$R2220 <- CAI1$`2250`/CAI1$`2000`
CAI1$R1620 <- CAI1$`1600`/CAI1$`2000`
CAI1$RSWIR <- CAI1$`1660`/CAI1$`R2260_2280`
CAI1$ROLI <- CAI1$`1660`/CAI1$R2220_2280

CAI1 <- CAI1[, desired_column]

write.csv(CAI1, file = paste0(path_to_data, "CAI_Soil.csv"), row.names = FALSE)


CAI <- rbind(CAI1, CAI)
CAI$Scan <- gsub(" ", "", CAI$Scan)

write.csv(CAI, file = paste0(path_to_data, "CAI_Combined.csv"), row.names = FALSE)


ggplot(CAI, aes(x = RWC, y = CAI, group = Type, color = Sample)) +
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

CAI$color_var <- ifelse(CAI$Sample == "Soil", "Soil", CAI$Type)
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

print(p6)
combined_plot <- (p3 + p4 + p5 + p6) + plot_layout(ncol=2)
combined_plot <- combined_plot & theme(legend.position = "bottom")
print(combined_plot)

ggsave(filename = paste0(path_to_plots, "ReflectRatio_RWC.png"), plot = combined_plot, width = 8.3, height = 5.8, dpi = 200)


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
                                paste(tidy_data$Sample, tidy_data$Type),
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

ggplot(tidy_data, aes(x = RWC, y = value, group = Type)) +
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

my_plot <- ggplot(tidy_data, aes(x = RWC, y = value, group = Type)) +
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
print(my_plot)
ggsave(filename = paste0(path_to_plots
                         , "Index_RWC.png"), plot = my_plot, width = 8.3, height = 5.8, dpi = 200)

##########################################################
##########################################################
#############  RWC ~ Reflectance (FACET)   ###############
library(ggplot2)
library(patchwork)

CAI$color_var <- ifelse(CAI$Sample == "Soil", "Soil", CAI$Type)
color_levels <- unique(CAI$color_var)
CAI$color_var <- factor(CAI$color_var, levels = c("Soil", color_levels[color_levels != "Soil"]))

write.csv(CAI, file = paste0(path_to_data, "CAI_test.csv"), row.names = FALSE)


df_long <- CAI %>%
  pivot_longer(cols = c("2000", "2250", "2090", "R2220_2260",
             "R1660_1690", "R2220_2280", "R2260_2280"),
              names_to = "Wvl", values_to = "Reflect")


# df_long1 <- CAI %>%
#   dplyr::filter(Sample == "Soil") %>%
#   pivot_longer(
#     cols = c("2160", "2190", "2180"), names_to = "Wvl", values_to = "Reflect")

# df_long2 <- CAI %>%
#   dplyr::filter(Sample == "Residue") %>%
#   pivot_longer(
#     cols = c("2000", "2250", "2090"), names_to = "Wvl", values_to = "Reflect")
# 
# df_long3 <- CAI %>%
#   pivot_longer(
#     cols = c("R2220_2260", "R1660_1690", "R2220_2280", "R2260_2280"), names_to = "Wvl", values_to = "Reflect")
# 
# columns = c("Sample", "Type", "Scan", "RWC", "color_var","Wvl", "Reflect")
# 
# df_long1 <-  df_long1[, columns]
# df_long2 <-  df_long2[, columns]
# df_long3 <-  df_long3[, columns]
# 
# df_long <- rbind(df_long1, df_long2, df_long3)

df_long <- df_long %>%
  mutate(Wvl = case_when(
    Wvl == 2160 ~ "R2.0",
    Wvl == 2180 ~ "R2.1",
    Wvl == 2190 ~ "R2.2",
    # Wvl == 2000 ~ "R2.0",
    # Wvl == 2090 ~ "R2.1",
    # Wvl == 2250 ~ "R2.2",
    Wvl == "R2220_2260" ~ "SWIR6",
    Wvl == "R2260_2280" ~ "SWIR7",
    Wvl == "R1660_1690" ~ "OLI6",
    Wvl == "R2220_2280" ~ "OLI7",
    TRUE ~ "Other"
  ))

df_long$shape_value <- paste(df_long$Type, as.character(df_long$Wvl))

df_Soil <- df_long[df_long$Sample %in% "Soil", ]
df_Res <- df_long[df_long$Sample %in% "Residue", ]

for (soil in unique(df_Soil$Type)) {
  for (crp in unique(df_Res$Type)){
    # Filter dataframe based on multiple string values in the column
    values_to_include <- c(soil, crp)  # Values to filter for
    df <- df_long[df_long$Type %in% values_to_include, ]
    
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
        
        Wvl == "R2.0" ~ "R2.0",
        Wvl == "R2.1" ~ "R2.1",
        Wvl == "R2.2" ~ "R2.2",
        Wvl == "SWIR6" ~ "SWIR6",
        Wvl == "SWIR7" ~ "SWIR7",
        Wvl == "OLI6" ~ "OLI6",
        Wvl == "OLI7" ~ "OLI7",
        TRUE ~ "Other"
      ))
    
    df_forOLI$shape_value <- paste(df_forOLI$Type, as.character(df_forOLI$Wvl))
    
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
    
    ggsave(filename = paste0(path_to_plots, "RWC_Reflectance_same_CAI_ref_2000_2250_2090/", soil,"_", crp, ".png"), plot = combined_plot, width = 5, height = 8, dpi = 300)
  }
}

