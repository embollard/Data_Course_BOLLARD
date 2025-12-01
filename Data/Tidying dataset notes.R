# Reading in messy blood pressure excel file and saving to object 'dat'
dat = read_xlsx('/Users/Angie/Desktop/Data_Course_BOLLARD/Data/messy_bp.xlsx',
                skip = 3)
# Viewing the saved object "dat"
dim(dat)
View(dat)

dat = dat[-1, ]

dat %>% 
  select(-c('HR...9', 'HR...11', 'HR...13')) %>% View()

dat %>% 
  select(-starts_with('HR')) %>% View()

# Combining blood pressure from each visit and storing them as diastolic and systolic 
dat_bp = dat %>% 
  select(-starts_with('HR')) %>% 
  pivot_longer(starts_with('BP'),
               names_to = 'visit',
               values_to = 'bp') %>% 
  mutate(visit = case_when(visit == 'BP...8' ~ 1,
                           visit == 'BP...10' ~ 2,
                           visit == 'BP...12' ~ 3)) %>% 
  separate(bp, c('sys', 'dia'), convert = T) %>% View()

# Creating object dat_hr from combining bp visits
dat_hr = dat %>% 
  clean_names() %>% 
  select(-starts_with('BP')) %>% 
  pivot_longer(starts_with('HR'),
               names_to = 'visit',
               values_to = 'hr') %>% 
  mutate(visit = case_when(visit == 'HR...9' ~ 1,
                           visit == 'HR...11' ~ 2,
                           visit == 'HR...13' ~ 3)) 
install.packages("dplyr")
library(dplyr)
# Creating object df_2 to join dat_bp and dat_hr
df_2 = full_join(dat_bp, dat_hr) #'full_join' applied to an object of class "NULL"
View(df_2)