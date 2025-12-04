# Load the necessary libraries
library(dplyr)
library(ggplot2)

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
grad_data$rank <- factor(grad_data$rank)

# Re-check the structure to confirm the changes
print(str(grad_data))