library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
file.path("C:/Users/Angie/Desktop/Data_Course_BOLLARD/Assignments/Assignment_7")
Utah_Religions_by_County = read_csv("Utah_Religions_by_County.csv")
View(Utah_Religions_by_County)
colnames(Utah_Religions_by_County)

# Display the initial structure of the data
print("Initial Data Structure:")
str(Utah_Religions_by_County)

# Define the columns that represent individual religious groups
religious_columns = c(
  "Assemblies of God",           
  "Episcopal Church",             
  "Pentecostal Church of God",   
  "Greek Orthodox",               
  "LDS",                         
  "Southern Baptist Convention", 
  "United Methodist Church",     
  "Buddhism-Mahayana",           
  "Catholic",                    
  "Evangelical",                 
  "Muslim",                      
  "Non Denominational",          
  "Orthodox"
)

# Tidy the data: Pivot the religion columns to long format
utah_tidy = Utah_Religions_by_County %>%
  pivot_longer(
    cols = all_of(religious_columns), # This uses the corrected vector
    names_to = "Religious_Group",
    values_to = "Proportion"
  ) %>%
  select(
    County, 
    Pop_2010, 
    Religious, 
    `Non-Religious`, 
    Religious_Group, 
    Proportion
  )

# Display the structure of the cleaned, tidy data
print("Tidy Data Structure:")
str(utah_tidy)
print("First few rows of tidy data:")
print(head(utah_tidy))

# Explore the cleaned data set with figures
print("Figure 1: Distribution of County Population (Pop_2010)")
utah_tidy %>%
  # Use Pop_2010 once per county (filter out duplicates caused by pivot)
  distinct(County, Pop_2010) %>%
  ggplot(aes(x = Pop_2010)) +
  geom_histogram(binwidth = 100000, fill = "lightgreen", color = "black") +
  
  # Add the scale function to format the x-axis labels
  scale_x_continuous(
    labels = label_number(scale = 1e-3, suffix = "K")
  ) +
  
  labs(
    title = "Distribution of County Population in Utah (2010)",
    x = "County Population", # No need to write 'K' here, the labels handle it
    y = "Number of Counties"
  ) +
  
  # Apply the theme elements for centering and bolding the title
  theme(
    plot.title = element_text(
      hjust = 0.5, # Centers the title
      face = "bold"
    )
  )
# The plot shows a strong skew towards smaller counties

# Distribution of Religious proportions
print("Figure 2: Distribution of Proportions for Major Religious Groups")
utah_tidy %>%
  # Filter for the major group: LDS, Catholic, Non-Religious
  filter(Religious_Group %in% c("LDS", "Catholic", "Non-Religious")) %>%
  ggplot(aes(x = Proportion, fill = Religious_Group)) +
  geom_histogram(
    alpha = 0.7, 
    position = "identity", 
    bins = 15,
    color = "black"  # Retaining the black outline for clarity
  ) +
  labs(
    title = "Distribution of Proportions by Major Group",
    x = "Proportion of County Population",
    y = "Count of County Group Observations"
  ) +
  theme_minimal() + 
  scale_fill_discrete(name = "Group") +
  # ADD THIS BLOCK to center the title
  theme(
    plot.title = element_text(
      hjust = 0.5, # Centers the title
      face = "bold" # Optional: make it bold
    )
  )
# This plot confirms LDS is a major group with high proportions, and others are lower.

# Question 1: Does Population of a county correlate with the
# proportion of any specific religious group in that county?
# A scatter plot of Pop_2010 vs. Proportion, faceted by 
# Religious_Group will visually answer this.
print("Figure 3: County Population vs. Religious Group Proportion")
ggplot(utah_tidy, aes(x = Pop_2010, y = Proportion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  
  # Format the X-axis labels to display in thousands (K)
  scale_x_continuous(
    labels = label_number(scale = 1e-3, suffix = "K")
  ) +
  
  facet_wrap(~ Religious_Group, scales = "free_y", ncol = 4) +
  labs(
    title = "County Population vs. Proportion of Religious Group",
    x = "County Population", 
    y = "Proportion of County Population"
  ) +
  theme_bw() +
  
  # Apply Theme Elements
  theme(
    plot.title = element_text(
      hjust = 0.5, # Centers the plot title
      face = "bold"
    ),
    # ROTATE THE X-AXIS LABELS (NUMBERS) 90 DEGREES
    axis.text.x = element_text(
      angle = 90, 
      vjust = 0.5, # Centers the text vertically (relative to the axis line)
      hjust = 1    # Aligns the rotated text to the right, ensuring it touches the tick mark
    )
  )
# This figure allows visual inspection for any trend 
# between population size and the proportion of each religious group.

# Correlation indices 
print("Correlation Indices: County Population vs. Proportion of Specific Religious Groups")

# Calculate the correlation for each religious group
pop_corr <- utah_tidy %>%
  group_by(Religious_Group) %>%
  summarise(
    Correlation_Pop = cor(Pop_2010, Proportion, use = "pairwise.complete.obs")
  ) %>%
  arrange(desc(abs(Correlation_Pop))) # Sort by strength of correlation

print(pop_corr)
# The correlation indices (r) will show the strength and direction of the linear 
# relationship. 

# Question: “Does proportion of any specific religion in a given county correlate
# with the proportion of non-religious people?”
# A scatter plot of 'Non-Religious' proportion vs. 'Proportion' 
# faceted by 'Religious_Group' will show this relationship.
print("Figure 4: Specific Religion Proportion vs. Non-Religious Proportion")
ggplot(utah_tidy, aes(x = Proportion, y = `Non-Religious`)) + # Note the backticks for 'Non-Religious'
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add a linear trend line
  facet_wrap(~ Religious_Group, scales = "free", ncol = 4) +
  labs(
    title = "Specific Religion Proportion vs. Non-Religious Proportion",
    x = "Proportion of Specific Religious Group",
    y = "Proportion of Non-Religious People"
  ) +
  theme_bw()
# This figure visually highlights which religious groups tend to be in counties 
# with higher or lower non-religious populations. 

## 5.2 Correlation Indices for Question 2
print("Correlation Indices: Specific Religion Proportion vs. Non-Religious Proportion")

# Calculate the correlation for each religious group
non_religious_corr <- utah_tidy %>%
  group_by(Religious_Group) %>%
  summarise(
    Correlation_NonReligious = cor(Proportion, `Non-Religious`, use = "pairwise.complete.obs")
  ) %>%
  arrange(desc(Correlation_NonReligious)) # Sort by correlation value

print(non_religious_corr)
# The correlation indices will likely show a strong negative correlation for LDS 
# (counties high in LDS are low in Non-Religious) and potentially positive for 
# other,smaller groups that might be associated with larger,more diverse counties.