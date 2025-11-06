library(tidyverse)
library(ggplot2)
library(gganimate)

# Read in data set
dat = read.csv("C:/Users/Angie/Desktop/Data_Course_BOLLARD/Exam_2/unicef-u5mr.csv")
View(dat)
headers = colnames(dat)
print(headers)

# Put into tidy format
u5mr_tidy = dat %>%
  # Pivot the data from wide to long format
  pivot_longer(
    cols = starts_with("U5MR."), # Select all columns starting with "U5MR."
    names_to = "Year",           # Name the new column for the old column names 'Year'
    values_to = "U5MR"           # Name the new column for the values 'U5MR'
  ) %>%
  # Clean up the Year column: remove the "U5MR." prefix and convert to numeric
  mutate(
    Year = as.numeric(str_replace(Year, "U5MR.", ""))
  ) %>%
  # Filter out any rows with missing U5MR data to prevent plot errors
  filter(!is.na(U5MR))

# View the first few rows of the tidy data (optional)
head(u5mr_tidy)

# --- Create the Plot ---

# Generate a line plot for each country, faceted by continent
ggplot(u5mr_tidy, aes(x = Year, y = U5MR, color = CountryName)) +
  geom_line() + # Creates a line plot for each country
  facet_wrap(~ Continent) + # Creates a grid of plots, one for each continent
  labs(
    title = "Under 5 Mortality Rate (U5MR) Over Time by Continent",
    x = "Year",
    y = "U5MR (deaths per 1000 live births)",
    color = "Country"
  ) +
  theme_minimal() +
  # Hide the legend because it is too large with many countries
  theme(legend.position = "none")

# Save the last generated plot to a file
ggsave("u5mr_plot.png", width = 10, height = 7, units = "in", dpi = 300)

#############################################################################

# Read in data set
dat = read.csv("C:/Users/Angie/Desktop/Data_Course_BOLLARD/Exam_2/unicef-u5mr.csv")
View(dat)
headers = colnames(dat)
print(headers)

# Put into tidy format
u5mr_tidy <- u5mr_data_raw %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = as.numeric(str_replace(Year, "U5MR.", ""))
  ) %>%
  filter(!is.na(U5MR), !is.na(Continent)) 

# --- Aggregate Data to find the Mean U5MR per Continent per Year ---
u5mr_mean_by_continent <- u5mr_tidy %>%
  # Group by continent and year
  group_by(Continent, Year) %>%
  # Calculate the mean U5MR for each group
  summarise(
    Mean_U5MR = mean(U5MR, na.rm = TRUE),
    .groups = 'drop' # Ungroup automatically after summarizing
  )
# View the aggregated data
head(u5mr_mean_by_continent)

# --- Create the Plot ---

# Generate a single line plot with lines colored by continent
ggplot(u5mr_mean_by_continent, aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(size = 1.2) + # Add lines, make them a bit thicker
  labs(
    title = "Mean Under-5 Mortality Rate (U5MR) by Continent Over Time",
    x = "Year",
    y = "Mean U5MR (deaths per 1000 live births)",
    color = "Continent" # Title the legend
  ) +
  scale_color_brewer(palette = "Set2") + # Use a nice color palette
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# --- Save the Plot ---
ggsave("BOLLARD_Plot_2.png", width = 10, height = 6, units = "in", dpi = 300)

#########################################################################################
library(tidyverse)
library(broom)   # For tidying model outputs
install.packages("AICcmodavg")
library(AICcmodavg) # For model comparison table

# Read in data set
dat = read.csv("C:/Users/Angie/Desktop/Data_Course_BOLLARD/Exam_2/unicef-u5mr.csv")
View(dat)
headers = colnames(dat)
print(headers)

# Put into tidy format
u5mr_tidy <- dat %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = as.numeric(str_replace(Year, "U5MR.", "")),
    # Ensure Continent is treated as a factor for modeling purposes
    Continent = as.factor(Continent)
  ) %>%
  # Filter out NAs which cause issues for linear models
  filter(!is.na(U5MR), !is.na(Continent))

# --- Create the Three Models ---

# Model 1: U5MR accounted for only by Year (simple linear regression)
# Formula: U5MR ~ Year
mod1 <- lm(U5MR ~ Year, data = u5mr_tidy)

# Model 2: U5MR accounted for by Year and Continent (additive effects)
# Formula: U5MR ~ Year + Continent
mod2 <- lm(U5MR ~ Year + Continent, data = u5mr_tidy)

# Model 3: U5MR accounted for by Year, Continent, and their interaction
# Formula: U5MR ~ Year * Continent 
mod3 <- lm(U5MR ~ Year * Continent, data = u5mr_tidy)

# Method 1: Compare R-squared (variance explained) and Residual Standard Error (RSE)
cat("--- Model Summary Comparison ---\n")
list(
  mod1 = glance(mod1) %>% select(r.squared, adj.r.squared, sigma) %>% mutate(model = "mod1"),
  mod2 = glance(mod2) %>% select(r.squared, adj.r.squared, sigma) %>% mutate(model = "mod2"),
  mod3 = glance(mod3) %>% select(r.squared, adj.r.squared, sigma) %>% mutate(model = "mod3")
) %>%
  bind_rows() %>%
  print()


# Method 2: Use AICcmodavg package for formal comparison using AIC, which penalizes complexity
cat("\n\n--- AIC/AICc and Delta AIC Comparison ---\n")
model_list <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3)
aictab(model_list, second.ord = TRUE) %>% print()

# Method 3: Use the built-in R anova() function to compare models sequentially
# This tests if the added complexity in the model is statistically significant
cat("\n\n--- Sequential ANOVA Comparison (F-test) ---\n")
anova(mod1, mod2, mod3) %>% print()

# --- Explanation of the Best Model ---

# Explanation: 
# Based on the AICc table and R-squared values, 'mod3' is the best model. 
# Mod3 has the highest R-squared (explains the most variance in U5MR), the lowest Residual Standard Error (sigma), 
# and the lowest AICc value (Akaike Information Criterion, which balances fit and complexity). 
# The low Delta_AICc for mod3 confirms it is significantly better than mod1 and mod2. 
# The ANOVA table also shows that the addition of Continent (mod2 vs mod1)
# and the addition of the interaction term (mod3 vs mod2) both result in a significantly better fit (p-value is very low). 
# This means that not only does the U5MR decrease over time, and the baseline rate changes by continent, 
# but the *rate* at which U5MR decreases over time is different for each continent.

##########################################################################################

library('broom')
library("patchwork")
library('ggplot2')
library('dplyr')

# Read in data set
dat = read.csv("C:/Users/Angie/Desktop/Data_Course_BOLLARD/Exam_2/unicef-u5mr.csv")
headers = colnames(dat)
print(headers)

# Put into tidy format
u5mr_tidy = dat %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = as.numeric(str_replace(Year, "U5MR.", "")),
    Continent = as.factor(Continent)
  ) %>%
  filter(!is.na(U5MR), !is.na(Continent))

# --- Define the Models ---
mod1 = lm(U5MR ~ Year, data = u5mr_tidy)
mod2 = lm(U5MR ~ Year + Continent, data = u5mr_tidy)
mod3 = lm(U5MR ~ Year * Continent, data = u5mr_tidy)

# --- Generate individual plot objects using the 'long' prediction data ---
prediction_data_base = expand.grid(
  Year = seq(min(u5mr_tidy$Year), max(u5mr_tidy$Year), by = 1),
  Continent = unique(u5mr_tidy$Continent)
) %>%
  mutate(
    pred_mod1 = predict(mod1, newdata = .),
    pred_mod2 = predict(mod2, newdata = .),
    pred_mod3 = predict(mod3, newdata = .)
  ) %>%
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "Model",
    values_to = "Prediction"
  ) %>%
  mutate(
    Model = recode(Model, 
                   "pred_mod1" = "Model 1 (Year only)",
                   "pred_mod2" = "Model 2 (Year + Continent)",
                   "pred_mod3" = "Model 3 (Year * Continent)")
  )


# Create the base ggplot call with the raw data
base_plot = ggplot(u5mr_tidy, aes(x = Year, y = U5MR)) +
  geom_point(alpha = 0.05, color = "gray") +
  facet_wrap(~ Continent) +
  theme_minimal() +
  labs(y = "U5MR", x = "Year")

# Now add the specific prediction lines and titles for each model using 'patchwork'
p1 = base_plot + 
  geom_line(data = filter(prediction_data_base, Model == "Model 1 (Year only)"), 
            aes(y = Prediction), color = "dodgerblue", size = 1) + 
  labs(title = "M1: Year Only Fit")

p2 = base_plot + 
  geom_line(data = filter(predictions_long, Model == "Model 2 (Year + Continent)"), 
            aes(y = Prediction), color = "dodgerblue", size = 1) + 
  labs(title = "M2: Additive Fit")

p3 = base_plot + 
  geom_line(data = filter(predictions_long, Model == "Model 3 (Year * Continent)"), 
            aes(y = Prediction), color = "dodgerblue", size = 1) + 
  labs(title = "M3: Interactive Fit")


# --- Combine the plots into a single image using patchwork ---

# Arrange the plots side-by-side using the | operator
combined_plot = p1 | p2 | p3

# Add a unifying title to the whole arrangement
final_plot = combined_plot + plot_annotation(
  title = 'Comparison of U5MR Model Predictions by Continent',
  theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
)

# Print the final combined plot
print(final_plot)

# --- Save the final combined plot ---
ggsave("combined_model_comparison_plot.png", plot = final_plot, width = 18, height = 7, units = "in", dpi = 300)

################################################################################################################
# BONUS- Create a new data frame for the prediction
# Use the correct, existing Continent name from your data's factor levels
ecuador_2020_data = data.frame(
  Year = 2020,
  Continent = as.factor("Americas")
)

# Predict the U5MR using mod3
predicted_u5mr = predict(mod3, newdata = ecuador_2020_data)

# Print the predicted value
print(predicted_u5mr)

#Prediction error was 34.7%

# To correct this prediction,we can account for non-linear trends using polynomial terms
mod3 = lm(U5MR ~ poly(Year, 2) * Continent, data = u5mr_tidy)

# This correction gives a prediction of 14.21 which is a error rate of 9.38%