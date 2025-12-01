# Install packages 
install.packages("survival")
install.packages("survminer")
install.packages("ggsurvplot")
install.packages("gridExtra")
library(tidyverse)
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)
library(gridExtra)

# 1. Import data set
cancer_data = read.csv("clinical_data_breast_cancer.csv")
View(cancer_data)
colnames(cancer_data)

# 2. Define the key columns for your analysis:
# Therapeutic Targets: ER.Status, PR.Status, HER2.Final.Status
# Cancer Stage: Converted.Stage or AJCC.Stage 
# Note: 'Converted.Stage' is often better for a simplified analysis if provided.
key_cols <- c(
  "Converted.Stage", 
  "ER.Status", 
  "PR.Status", 
  "HER2.Final.Status"
)  

# 3. Perform the simplification, grouping by stage and target status:
simplified_analysis <- cancer_data %>%
  # Select only the relevant columns to focus the data
  select(all_of(key_cols), "Complete.TCGA.ID") %>%
  
  # Group the data by the combination of Stage and Target Status
  group_by(Converted.Stage, ER.Status, PR.Status, HER2.Final.Status) %>%
  
  # Summarize the count of patients in each group
  summarise(
    Patient_Count = n(), # Counts the number of rows in each group
    # You can add other summary statistics here, e.g.,
    # Mean_Age = mean(Age.at.Initial.Pathologic.Diagnosis, na.rm = TRUE)
    .groups = 'drop' # Ungroup the data after summarizing
  ) %>%
  
  # Order the results for better readability
  arrange(Converted.Stage, Patient_Count)

# 4. Display the resulting table
print(simplified_analysis)

# --- Define TNBC based on status ---
# TNBC patients are:
# ER.Status == "Negative"
# PR.Status == "Negative"
# HER2.Final.Status == "Negative"

# 5. Calculate TNBC and Total patients per stage
tnbc_proportion <- cancer_data %>%
  # Group by the primary variable: Cancer Stage
  group_by(Converted.Stage) %>%
  
  # Summarize the data to get two key counts:
  summarise(
    # Total number of patients in this stage
    Total_Patients = n(), 
    
    # Count of TNBC patients in this stage
    TNBC_Patients = sum(
      ER.Status == "Negative" &
        PR.Status == "Negative" &
        HER2.Final.Status == "Negative",
      na.rm = TRUE
    ),
    .groups = 'drop' # Ungroup the data after summarizing
  ) %>%
  
  # 5. Calculate the proportion
  mutate(
    Proportion_TNBC = TNBC_Patients / Total_Patients,
    # Format the proportion as a percentage for easier reading (optional)
    Percentage_TNBC = paste0(round(Proportion_TNBC * 100, 2), "%")
  ) %>%
  
  # Select and arrange the final columns
  select(
    Converted.Stage, 
    TNBC_Patients, 
    Total_Patients, 
    Proportion_TNBC, 
    Percentage_TNBC
  ) %>%
  arrange(Converted.Stage)

View(final_data)

# 7. Display the resulting table
print(tnbc_proportion)

---------------------------------------------------------------------------------------
# 8. Ensure the stage variable is a factor and the survival event status is correct
cancer_data <- cancer_data %>%
  mutate(
    Converted.Stage = factor(Converted.Stage),
    # KM analysis works best with a factor/grouping variable
    OS.event = as.numeric(OS.event) # Ensure event is numeric (0/1)
  ) %>%
  # Remove rows with NA in key survival columns for a clean analysis
  filter(!is.na(OS.Time) & !is.na(OS.event) & !is.na(Converted.Stage))

# 9. Fit the Kaplan-Meier Survival Model
# The formula format is Surv(Time, Event) ~ Grouping_Variable
km_fit_by_stage <- survfit(
  Surv(OS.Time, OS.event) ~ Converted.Stage, 
  data = cancer_data
)

# 10. Create the data frame for ggplot2 using the 'survfit' structure
# Use the 'strata' element from the fit object. This element is a named vector 
# where the names define the strata boundaries within the main vectors.
strata_names <- names(km_fit_by_stage$strata)
strata_lengths <- km_fit_by_stage$strata

# Create a vector of stage names that repeats the name for the number of rows in that stratum
stage_vector <- rep(strata_names, times = strata_lengths)

# Now, create the data frame using all vectors which are guaranteed to be the same length
surv_data_long <- data.frame(
  time = km_fit_by_stage$time,
  surv = km_fit_by_stage$surv,
  lower = km_fit_by_stage$lower,
  upper = km_fit_by_stage$upper,
  # Use the 'n.risk' vector directly without any indexing.
  # This vector's length (97) should match all other vectors.
  n.risk = km_fit_by_stage$n.risk, 
  strata = stage_vector 
)

# 11. Rename the stage column and clean up the factor names
surv_data_long <- surv_data_long %>%
  rename(Converted.Stage = strata) %>%
  mutate(
    # Clean up the stage names (removes "Converted.Stage=")
    Converted.Stage = gsub("Converted.Stage=", "", Converted.Stage),
    Converted.Stage = factor(Converted.Stage)
  )

# 12. Visualize the Kaplan-Meier Curves using ggplot2 (Same as before)
km_plot_ggplot <- ggplot(surv_data_long, aes(x = time, y = surv, color = Converted.Stage)) +
  # Use geom_step to create the characteristic step-wise KM curve
  geom_step(size = 1) + 
  
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Converted.Stage), alpha = 0.1, color = NA) +
  
  # Set labels and title
  labs(
    title = "Kaplan-Meier Survival Curves by Breast Cancer Stage",
    x = "Time in Days",
    y = "Overall Survival Probability",
    color = "AJCC Stage",
    fill = "AJCC Stage"
  ) +
  
  # Customize the plot appearance
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_classic() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Display the plot
print(km_plot_ggplot)