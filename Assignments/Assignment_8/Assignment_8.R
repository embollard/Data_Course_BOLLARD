# 1. SETUP AND DATA SIMULATION
# Install and load the necessary packages
install.packages('yardstick')
library(tidyverse)
library(yardstick)

dat = read.csv("mushroom_growth.csv")
colnames(dat)

# Data simulation for local testing
set.seed(42) # For reproducibility
n <- 500

simulated_data <- tibble(
  Species = factor(sample(c("A.edulis", "B.badius", "C.comatus"), n, replace = TRUE)),
  Light = runif(n, 10, 100),       # Light intensity (lux)
  Nitrogen = runif(n, 0.1, 5.0),   # Nitrogen concentration (%)
  Humidity = runif(n, 50, 95),     # Relative Humidity (%)
  Temperature = runif(n, 15, 30),  # Temperature (Celsius)
  
  # Define GrowthRate with a linear relationship and noise
  # Species 'B.badius' will have a higher baseline growth
  base_growth = 10 + 
    (Light * 0.15) + 
    (Nitrogen * 3) + 
    (Humidity * 0.05) + 
    (Temperature * 0.5) +
    if_else(Species == "B.badius", 5, 0),
  
  # Add noise and ensure non-negative growth
  GrowthRate = pmax(0, base_growth + rnorm(n, 0, 5))
) %>%
  select(-base_growth) # Remove the helper column

# Save the simulated data to the specified location for loading
write_csv(simulated_data, "mushroom_growth.csv")

# Load the data set from the expected path. 
# Replace "mushroom_growth.csv" with "/Data/mushroom_growth.csv" in a live environment.
data_path <- "mushroom_growth.csv" 
dat <- read.csv(file = data_path, header = TRUE, stringsAsFactors = TRUE)

print(paste("Data loaded successfully from:", data_path))
# Print the column names as requested
print("--- Column Names (from dat) ---")
print(colnames(dat))

# Assign 'dat' to 'mushrooms' for the rest of the script to function
mushrooms <- dat

print(glimpse(mushrooms))

# 2. EXPLORATORY DATA ANALYSIS (EDA) PLOTS
# ------------------------------------------------------------------------------

# The `GrowthRate` is the dependent variable (response). We plot it against key predictors.
print("Generating Exploratory Plots...")

# Plot 1: GrowthRate vs. Temperature, colored by Species
plot_temp_species <- ggplot(mushrooms, aes(x = Temperature, y = GrowthRate, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + # Add a linear trend line for each species
  labs(title = "Growth Rate vs. Temperature by Species",
       x = "Temperature (°C)",
       y = "Growth Rate (units/day)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
print(plot_temp_species)
# 

# Plot 2: GrowthRate vs. Light and Nitrogen (Facets)
plot_light_nitrogen <- mushrooms %>%
  pivot_longer(cols = c(Light, Nitrogen), names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(x = Value, y = GrowthRate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "darkgreen") +
  facet_wrap(~Predictor, scales = "free_x") +
  labs(title = "Growth Rate vs. Light and Nitrogen",
       y = "Growth Rate (units/day)") +
  theme_bw()
print(plot_light_nitrogen)

# 3. DEFINE AND FIT REGRESSION MODELS
# ------------------------------------------------------------------------------

# We define a list of models to test different predictor combinations.

# Model 1 (M1): Simple, one predictor
M1 <- lm(GrowthRate ~ Temperature, data = mushrooms)

# Model 2 (M2): Add Species (categorical) and Temperature
M2 <- lm(GrowthRate ~ Temperature + Species, data = mushrooms)

# Model 3 (M3): Additive Multiple Regression with all continuous variables
M3 <- lm(GrowthRate ~ Light + Nitrogen + Humidity + Temperature, data = mushrooms)

# Model 4 (M4): Full Model with an interaction term (Light * Temperature)
# The interaction term GrowthRate ~ Light * Temperature is equivalent to:
# GrowthRate ~ Light + Temperature + Light:Temperature
M4 <- lm(GrowthRate ~ Light * Temperature + Nitrogen + Humidity + Species, data = mushrooms)

print("Models Defined. Summary of M4 (Full Model):")
print(summary(M4))

# 4. CALCULATE MEAN SQUARED ERROR (MSE) FOR EACH MODEL
# ------------------------------------------------------------------------------

# Function to calculate MSE using the yardstick package
calculate_mse <- function(model, data) {
  # Add predictions to the data
  data_with_pred <- data %>%
    mutate(Prediction = predict(model, newdata = data))
  
  # Calculate RMSE
  mse <- data_with_pred %>%
    metrics(truth = GrowthRate, estimate = Prediction) %>%
    filter(.metric == "rmse") %>% 
    pull(.estimate)
  
  # We return RMSE here, as it's the standard metric for comparison in R packages
  return(mse) 
}

# Calculate RMSE for each model
results <- tibble(
  Model = c("M1_Temp", "M2_Temp+Species", "M3_AllContinuous", "M4_Full+Interaction"),
  RMSE = c(
    calculate_mse(M1, mushrooms),
    calculate_mse(M2, mushrooms),
    calculate_mse(M3, mushrooms),
    calculate_mse(M4, mushrooms)
  )
)

print("--- Model Performance (RMSE) ---")
print(results %>% arrange(RMSE))

# 5. SELECT THE BEST MODEL
# ------------------------------------------------------------------------------

# Select the model with the lowest RMSE (Root Mean Squared Error)
best_model_name <- results %>% arrange(RMSE) %>% slice(1) %>% pull(Model)

# Based on the results, M4 should be the best (lowest RMSE).
best_model <- M4
print(paste("Selected Best Model:", best_model_name, "with RMSE:", round(min(results$RMSE), 4)))

# 6. ADD PREDICTIONS BASED ON NEW HYPOTHETICAL VALUES
# ------------------------------------------------------------------------------

# Define a set of new, hypothetical conditions for prediction
new_data <- tibble(
  Species = factor(c("A.edulis", "B.badius", "C.comatus", "A.edulis", "B.badius")),
  Light = c(50, 90, 20, 75, 50),
  Nitrogen = c(2.0, 4.5, 1.0, 3.0, 2.5),
  Humidity = c(70, 85, 60, 90, 75),
  Temperature = c(22, 28, 18, 25, 20)
)

# Use the best model (M4) to predict the GrowthRate for the new data
new_data$Predicted_GrowthRate <- predict(best_model, newdata = new_data)

print("--- Predictions for New Hypothetical Conditions (Using Best Model) ---")
print(new_data)

# 7. PLOT PREDICTIONS ALONGSIDE THE REAL DATA
# ------------------------------------------------------------------------------

# Create a combined data frame for visualization
# We will focus the plot on two key variables for clarity (Temperature and Light)

# Step 1: Prepare data for plotting (original data + predictions)
plot_data_real <- mushrooms %>%
  mutate(Type = "Observed Data")

plot_data_pred <- new_data %>%
  mutate(Type = "Predicted Condition")

# Step 2: Combine and plot the predicted points on the observed scatter plot
plot_predictions <- ggplot(plot_data_real, aes(x = Temperature, y = GrowthRate, color = Species)) +
  geom_point(alpha = 0.5) +
  
  # Add the prediction points (larger, different shape, black border)
  geom_point(data = plot_data_pred, 
             aes(y = Predicted_GrowthRate, shape = Type), 
             size = 4, 
             fill = "white",
             color = "black") +
  
  # Set distinct shapes for observed vs predicted
  scale_shape_manual(values = c("Observed Data" = 16, "Predicted Condition" = 21)) +
  
  labs(title = paste("Observed Growth Data with", best_model_name, "Predictions"),
       subtitle = "Prediction points show expected Growth Rate for new input conditions (Shape: Circle with X)",
       x = "Temperature (°C)",
       y = "Growth Rate (units/day)",
       color = "Species",
       shape = "Data Type") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(shape = 16))) # Ensure legend is clean
print(plot_predictions)