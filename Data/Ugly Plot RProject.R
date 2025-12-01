install.packages(c("gganimate", "gifski", "magick", "transformr", "png"))

honor = read.csv("ATLA-episodes-scripts.csv", stringsAsFactors = FALSE)

# View the first few rows
names(honor)
subset(honor, grepl("Zuko", character(), ignore.case = TRUE))

# Make sure we have the counts ready
honor_counts <- honor %>%
  mutate(Character = ifelse(is.na(Character) | Character == "", "Unknown", Character)) %>%
  group_by(Character) %>%
  summarise(honor_mentions = n()) %>%
  arrange(desc(honor_mentions))

# Animate
# If your dataset has episode numbers:
p <- ggplot(honor_episode, aes(x = Character, y = episode_mentions, color = Character)) +
  geom_point(size = 10, alpha = 0.5, position = position_jitter(width = 0.5, height = 0.5)) +
  labs(
    title = "HONOR MENTIONS!! SO MANY POINTS!! CAN YOU READ THIS??",
    x = "CHARACTERZZZ",
    y = "HONORRRRR!!!"
  ) +
  scale_color_manual(values = rainbow(nrow(honor_episode))) +
  theme(
    axis.text.x = element_text(angle = 75, vjust = 0.1, hjust = 1),
    panel.background = element_rect(fill = "darkorange4"), 
    panel.grid.major = element_line(color = "chartreuse", size = 2),
    panel.grid.minor = element_line(color = "burlywood4", size = 1),
    legend.position = "none"
  ) +
  transition_states(ep_number, transition_length = 2, state_length = 1) +
  ease_aes('linear')

animated_plot = animate(p, nframes = 150, fps = 10)

# Create a folder for frames
dir.create("frames_png", showWarnings = FALSE)

# Folder to save everything
save_folder = "C:/Users/Angie/Desktop/Data_Course_BOLLARD/Data"
dir.create(save_folder, showWarnings = FALSE)

# --- Save as GIF ---
anim = animate(p, nframes = 150, fps = 15, width = 800, height = 600)
anim_save(file.path(save_folder, "honor_strobe.gif"), animation = anim)

# --- Save as TIFF frames ---
# Folder for TIFF frames
tiff_folder <- file.path(save_folder, "tiff_frames")
dir.create(tiff_folder, showWarnings = FALSE)

# Save frames as TIFF using file_renderer
animate(
  p,
  nframes = 150,
  fps = 15,
  renderer = file_renderer(
    prefix = file.path(tiff_folder, "frame_"),
    overwrite = TRUE,
    # Specify TIFF extension
    file_type = "tiff"
  ),
  width = 800,
  height = 600
)

# --- Save PNG frames ---
png_folder <- "C:/Users/Angie/Desktop/Data_Course_BOLLARD/Data/frames_png"
dir.create(png_folder, showWarnings = FALSE)

# Render and save all frames as PNG
frames <- animate(
  p,
  nframes = 150,
  fps = 10,
  width = 800,
  height = 600,
  renderer = magick_renderer()
)

# Save each frame as a PNG in your folder
for (i in seq_along(frames)) {
  img_path <- file.path(png_folder, sprintf("frame_%03d.png", i))
  image_write(frames[i], path = img_path, format = "png")
}