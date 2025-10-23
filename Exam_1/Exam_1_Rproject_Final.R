getwd()
setwd("C:/Users/Angie/Desktop/Data_Course_BOLLARD/BIOL3100_Exams/Exam_1")
## Reading in csv and creating a data frame
df = read.csv('cleaned_covid_data.csv')
## Making sure State column is treated as a character
df$Province_State <- as.character(df$Province_State)
## Use subset() with grepl() function to find states beginning with "A"
A_states <- subset(df, grepl("^A", Province_State))
## Convert Date column into a date format
A_states$Last_Update <- as.Date(A_states$Last_Update, format = "%Y-%m-%d")
## Ensure deaths column is numeric
A_states$Deaths <- as.numeric(A_states$Deaths)
## Header Names 
names(A_states)
## Filter the data and save to new object
A_states <- df %>%
  filter(startsWith(Province_State, "A"))
## Create plot object
A_states %>% 
  ggplot(aes(x = Last_Update,
             y = Deaths,
             color = Province_State)) +
  labs(x = 'Date', y = 'Deaths') +
  geom_point() +
  labs(title = "COVID-19 Deaths Over Time for States Starting with 'A'")

View(A_states)

## group by Province_State and summarize
state_max_fatality_rate <- df %>% 
  group_by(Province_State) %>% 
  summarise(Max_CFR = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  ungroup()
## View new data frame
print(state_max_fatality_rate)

## Use state_max_fatality_rate to create a plot
state_max_fatality_rate %>% 
  ggplot(aes(x = Province_State,
             y = Max_CFR)) +
  geom_point() +
  labs(x = "Provence or State", y = "Maximum CFR") +
  theme(
    axis.text.x = element_text(
      angle = 45,        # Specify the angle in degrees
      hjust = 1,         # Horizontal justification
      vjust = 1,         # Vertical justification
      size = 10          # Optional: Adjust font size
    )
  )
labs(title = "Maximum Fatality Rate By State")

## Sum the cumulative deaths across all states for each day.
us_cumulative_deaths <- df %>%
  mutate(Date = as.Date(Last_Update)) %>%
  group_by(Date) %>% 
  summarise( Total_US_Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  ungroup()

## Plot the us_cumulative_deaths
cumulative_deaths_plot <- us_cumulative_deaths %>%
  ggplot(aes(x = Date, y = Total_US_Deaths)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(color = "#D55E00", size = 0.5) +
  labs(
    title = "Cumulative COVID-19 Deaths for the Entire US",
    subtitle = "Aggregated from State-Level Data",
    x = "Date",
    y = "Total Cumulative Deaths"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
print(cumulative_deaths_plot)