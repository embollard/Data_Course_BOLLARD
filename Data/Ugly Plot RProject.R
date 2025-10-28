read.csv("ATLA-episodes-scripts.csv")
head("ATLA-episodes-scripts.csv")
colnames("ATLA-episodes-scripts.csv")
honor = read.csv("ATLA-episodes-scripts.csv", stringsAsFactors = FALSE)

# View the first few rows
head(honor)
names(honor)
subset(honor, grepl("Zuko", character(), ignore.case = TRUE))

honor_counts <- honor |>
  mutate(Character = ifelse(is.na(Character) | Character == "", "Unknown", Character)) |>
  group_by(Character) |>
  summarise(honor_mentions = n()) |>
  arrange(desc(honor_mentions))

# Make sure we have the counts ready
honor_counts <- honor %>%
  mutate(Character = ifelse(is.na(Character) | Character == "", "Unknown", Character)) %>%
  group_by(Character) %>%
  summarise(honor_mentions = n()) %>%
  arrange(desc(honor_mentions))

# Add highlight column (for Zuko)
honor_counts <- honor_counts %>%
  mutate(highlight = ifelse(grepl("Zuko", Character, ignore.case = TRUE), "Zuko", "Other"))

# Keep only top 20
top20 <- honor %>%
  mutate(Character = trimws(Character)) %>%                     # remove extra spaces
  filter(Character != "Unknown") %>%                            # remove unknowns
  slice_max(order_by = total_number, n = 20) %>%               # select top 20
  mutate(highlight = ifelse(Character == "Zuko", "Zuko", "Other"))

# Add a frame/order for animation
top20 <- honor %>%
  mutate(Character = trimws(Character)) %>%                     # remove spaces
  filter(Character != "Unknown") %>%                            # remove unknowns
  slice_max(order_by = total_number, n = 20) %>%               # top 20
  mutate(highlight = ifelse(Character == "Zuko", "Zuko", "Other"))

# Plot
# Animated scatter plot
p <- ggplot(top20, aes(x = total_number, 
                       y = reorder(Character, total_number),
                       color = highlight, 
                       size = total_number)) +
  geom_point(show.legend = FALSE) +
  geom_text(
    aes(label = total_number),
    hjust = -0.3, size = 4, color = "gray20",
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Zuko" = "firebrick", "Other" = "#007CC3")) +
  scale_size(range = c(4, 10)) +
  labs(
    title = "Top 20 Characters Who Say 'Honor' in Avatar: The Last Airbender",
    subtitle = "Points grow to show total mentions, labels appear at the end",
    x = "Mentions of 'Honor'",
    y = "Character"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  ) +
  transition_reveal(total_number) +             # points grow by x-value
  ease_aes('cubic-in-out')

#Manually patch gganimate's internal function
unlockBinding("format_frame", asNamespace("gganimate"))
assign("format_frame", function(i, nframes) {
  nc <- ceiling(log10(nframes + 1))
  sprintf(paste0("%0", nc, "d"), i)
}, envir = asNamespace("gganimate"))
lockBinding("format_frame", asNamespace("gganimate"))

# Animate
options(gganimate.frame_format = "%04d")
animate(
  p,
  nframes = 80,
  fps = 12,
  width = 800,
  height = 600,
  renderer = gifski_renderer("honor_animation.gif")
)

install.packages(c("gganimate", "gifski", "magick", "transformr", "png"))
