# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork) 
library(broom)

# Load the data using a relative path
grad_data = read.csv("GradSchool_Admissions.csv")

# Review the structure and first few rows of the data
print(head(grad_data))
print(str(grad_data))
print(summary(grad_data))

# Check for any missing values (should be 0)
print(sum(is.na(grad_data)))

# Convert 'admit' (outcome) and 'rank' (predictor) to factors
# admit: 0=failure, 1=success
grad_data$admit <- factor(grad_data$admit, 
                          levels = c(0, 1), 
                          labels = c("Denied", "Admitted"))

# rank: 1="top-tier" to 4="lowest-tier" (ordinal, but treated as categorical in glm)
# R will automatically set the lowest numerical level (1) as the reference category.
# Rank 1 (Top-tier) will be the reference level for the model.
grad_data$rank <- factor(grad_data$rank, 
                    levels = 1:4, 
                    labels = paste("Rank", 1:4))

# Re-check the structure to confirm the changes
print(str(grad_data))

# Overall summary statistics
print(summary(grad_data))

# Admission rates and average predictor scores by institution rank
rank_summary <- grad_data %>%
  group_by(rank) %>%
  summarise(
    Total_Applicants = n(),
    Admission_Rate = mean(admit == "Admitted"),
    Avg_GRE = mean(gre),
    Avg_GPA = mean(gpa)
  )
print(rank_summary)

# GRE Boxplot
p1 <- ggplot(grad_data, aes(x = admit, y = gre, fill = admit)) +
  geom_boxplot() +
  labs(title = "GRE Score by Admission Status", x = NULL, y = "GRE Score") +
  theme_minimal() +
  theme(legend.position = "none")

# GPA Boxplot
p2 <- ggplot(grad_data, aes(x = admit, y = gpa, fill = admit)) +
  geom_boxplot() +
  labs(title = "GPA by Admission Status", x = NULL, y = "GPA") +
  theme_minimal() +
  theme(legend.position = "none")

# ... code for p1 and p2 definition ...

# Display plots side-by-side using patchwork
p1 + p2 # This is the correct line for patchwork

# Full model fitting. We fit the full model
#, $\text{admit} \sim \text{gre} + \text{gpa} + \text{rank}$, 
# using the $\text{family = "binomial"}$ option in the $\text{glm()}$ function.
model_full <- glm(admit ~ gre + gpa + rank, data = grad_data, family = "binomial")

# Display the model summary
summary(model_full)

# Model Comparison (Likelihood Ratio Test)
# Reduced model without rank
model_reduced <- glm(admit ~ gre + gpa, data = grad_data, family = "binomial")

# Compare models using the Likelihood Ratio Test (ANOVA)
anova(model_reduced, model_full, test = "Chisq")

# Extract odds ratios and confidence intervals
OR_table <- tidy(model_full, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)

# Print the interpretation table
print(OR_table)
