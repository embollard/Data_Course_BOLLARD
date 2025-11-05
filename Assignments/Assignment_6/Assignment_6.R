library(tidyverse)
library(gganimate)
library(gifski)

# Read Dataset
dat = read_csv("C:/Users/Angie/Desktop/Data_Course_BOLLARD/Data/BioLog_Plate_Data.csv")
View(dat)
head(dat)

# 2. Clean and transform data into tidy (long) form
# Pivot the time-point columns (Hr_24, Hr_48, Hr_144) into long format
biolog_data_tidy = dat %>%
  pivot_longer(
    cols = starts_with("Hr_"), # Selects columns starting with "Hr_"
    names_to = "Time_point_Hr",
    values_to = "Absorbance"
  ) %>%
  
  # 3. Create a new column specifying whether a sample is from soil or water
  # Assuming "Sample ID" contains information to differentiate sample types.
  mutate(
    Sample_Type = ifelse(str_detect(`Sample ID`, "[Ss]oil"), "Soil", "Water"),
    # Convert Time_point string (e.g., "Hr_24") to a numeric value (e.g., 24)
    Time_point_Hr = parse_number(Time_point_Hr)
  ) %>%
  # Filter for dilution == 0.1 as required by the plot
  filter(Dilution == 0.1)

# View the head of the tidy data to confirm structure
print(head(biolog_data_tidy))
str(biolog_data_tidy)

# 4. Generate the plot
# This plot uses Time_point_Hr on the x-axis, Absorbance on the y-axis,
# and color/linetype to differentiate Sample_Type (Soil/Water).

ggplot(biolog_data_tidy, aes(x = Time_point_Hr, y = Absorbance, color = Sample_Type, group = Sample_Type)) +
  geom_smooth(se = FALSE, size = 0.7) +
  facet_wrap(~ Substrate, scales = "free_y") +
  scale_color_manual(values = c('Soil' = 'red', 'Water' = 'darkcyan'))+
  labs(
    title = "Absorbance over Time for Soil and Water Samples (Dilution 0.1)",
    x = "Time Points by Hour",
    y = "Absorbance",
) +
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    strip.text = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  # Customize color and linetype scales
  scale_color_manual(values = c("Soil" = "brown", "Water" = "blue")) +
  scale_linetype_manual(values = c("Soil" = "solid", "Water" = "dashed"))

# You can save the plot to a file using ggsave()
ggsave("biolog_plot_final.png", width = 8, height = 5)

#####################################################################################

# 1. Read the dataset
dat = read_csv("C:/Users/Angie/Desktop/Data_Course_BOLLARD/Data/BioLog_Plate_Data.csv")
# Define the specific Sample IDs we want to plot
selected_samples <- c("Clear_Creek", "Soil_1", "Soil_2", "Waste_Water")
# Clean, transform, and summarize the data
biolog_data_summary = dat %>%
  # Filter for the specific substrate "Itaconic Acid"
  filter(Substrate == "Itaconic Acid") %>%
  # Filter for the specific dilution (assuming 0.1)
  filter(Dilution == 0.1) %>%
  # Filter ONLY for the specific Sample IDs requested
  filter(`Sample ID` %in% selected_samples) %>%
  # Pivot the time-point columns (Hr_24, Hr_48, Hr_144) into long format
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time_point_Hr_String",
    values_to = "Absorbance"
  ) %>%
  mutate(
    # Convert Time_point string (e.g., "Hr_24") to a numeric value (e.g., 24)
    Time_point_Hr = parse_number(Time_point_Hr_String),
    # Ensure Sample ID is a factor for consistent plotting order
    `Sample ID` = factor(`Sample ID`, levels = selected_samples)
  ) %>%
  # Calculate the mean absorbance for all 3 replicates for each group
  # Grouping by the specific ID now
  group_by(Time_point_Hr, `Sample ID`) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = 'drop'
  )

# Check if data exists
if(nrow(biolog_data_summary) == 0) {
  stop("Error: No data found for the selected samples ('Clear_creek', 'Soil_1', 'Soil_2', 'Waste_Water') under 'Itaconic Acid' at 'Dilution == 0.1'. Check your CSV file values.")
}

# 3. Generate the animated plot object 'p'
# Now mapping 'Sample ID' to color and linetype
p <- ggplot(biolog_data_summary, aes(
  x = Time_point_Hr,
  y = Mean_Absorbance,
  color = `Sample ID`,
  linetype = `Sample ID` # Use different linetypes if desired
)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = 'Mean Absorbance of Itaconic Acid Samples over Time',
    subtitle = 'Time Point (Hours): {frame_along}',
    x = 'Time Point (Hours)',
    y = 'Mean Absorbance',
    color = 'Sample ID',
    linetype = 'Sample ID'
  ) +
  theme_minimal(base_size = 12) +
  # You can manually set colors for specific IDs if you like
  # scale_color_manual(values = c("Clear_creek" = "blue", "Soil_1" = "brown", ...))
  
  # Use transition_reveal to grow the line along the X-axis (Time_point_Hr)
  transition_reveal(along = Time_point_Hr) +
  ease_aes('linear')


# 4. Animate and save the plot
animated_plot <- animate(
  p,
  fps = 15,
  nframes = 45,
  width = 700,
  height = 450,
  renderer = gifski_renderer()
)

anim_save("animated_biolog_specific_samples_plot.gif", animated_plot)












