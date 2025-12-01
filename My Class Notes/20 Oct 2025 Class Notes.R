library(readxl)


dat = read_xlsx('C:/Users/Angie/Desktop/Data_Course_BOLLARD/Data/messy_bp.xlsx')
dat = read_xlsx('messy_bp.xlsx', skip = 3)
View(dat)

dat_bp = dat %>% 
  select(-starts_with("HR")) %>% 
  pivot_longer(starts_with('BP'), 
          names_to = 'visit',
          values_to = 'bp') %>% 
  mutate(visit = case_when(visit == 'BP...8' ~1,
                           visit == 'BP...9' ~2,
                           visit == 'BP...10' ~3)) %>% View()

# / = or
# & = and

# "Caucausian" and "WHITE" to "White"
df_2 %>% 
  mutate(race_fixed = case_when(Race == "Caucasian" ~ "White",
                                Race == "WHITE" ~ "White",
                                True ~ Race)) %>% View()

#renaming columns
colnames(df_2) = c('pat_id', 'Month_of_Birth', '')

install.packages('janitor')
library('janitor')

df_2 %>% 
  clean_names() %>% 
  View()
dim(dat)
View(dat)

dat_bp = dat %>% 
  select(-starts_with('HR')) %>% 
  pivot_longer(starts_with('BP'),
               names_to = 'visit',
               values_to = 'bp') %>% 
  mutate(visit = case_when(visit == 'BP...8' ~ 1,
                           visit == 'BP...10' ~ 2,
                           visit == 'BP...12' ~ 3)) %>% 
  separate(bp, c('sys', 'dia'), convert = T) %>% View()

dat_hr = dat %>% 
  clean_names() %>% 
  select(-starts_with('BP')) %>% 
  pivot_longer(starts_with('HR'),
               names_to = 'visit',
               values_to = 'hr') %>% 
  mutate(visit = case_when(visit == 'HR...9' ~ 1,
                           visit == 'HR...11' ~ 2,
                           visit == 'HR...13' ~ 3)) 

df_2 = full_join(dat_bp, dat_hr)
View(df_2)

df_2$Race %>% unique()


str_to_upper("Caucasian")
str_to_lower("WHITE")

df_2 %>% 
  mutate(race_fixed = case_when(Race == 'Caucasian' ~ 'White',
                                Race == 'WHITE' ~ 'White',
                                TRUE ~ Race)) %>% View()

df_2 %>% 
  mutate(Race = case_when(Race == 'Caucasian' | Race == 'WHITE'~ 'White',
                          TRUE ~ Race)) %>% head()

#| = or
#& = and 

df_2$`Day birth`

colnames(df_2) = c('pat_id', 'Month_of_birth', '')


library(janitor)

clean_names()
make_clean_names()


make_clean_names('# of cookies')

df3 = df_2 %>% 
  clean_names() 


View(df3)


## "Caucasian" and "WHITE" to "White"

# Combine birth date, month, year and make a new column for birthdate
df3 %>% 
  mutate(birthday = as.Date(paste(dat$`Year birth`, dat$`Month of birth`, dat$`Day birth`, sep = '-'))
View()

select(-c(year_birth, month_of_birth, day_birth)) %>% 
arrange(pat_id) %>% 
  mutate(new_id = rep(1:(nrow(df3)/3), each = 3)) %>% View()

#Making a plot of BP data
df3 %>% 
ggplot(aes(x = visit, 
           color = race)) +
  geom_path(aes(y = sys)) +
  geom_path(aes(y = dia)) +
  facet_wrap(~race)

df4 %>% 
  pivot_longer(c('sys', 'dia'), names_to = 'bp_type') +
                    values_to = "bp") %>% 
  ggplot(aes(x = visit, y = bp, color = 'bp_type')) +
  geom_path() +
  facet_grid(sex ~ race)

getwd()
bdat = read.csv('Bird_Measurements.csv')
View(bdat)

iwant = c('Family', 'Species_number', 'English_name', 'Species_name', 'Clutch_size', 'Egg_mass', 'Mating_System')

bdat %>% 
  select(iwant, starts_with("M_"), -ends_with('_N')) %>% 
  View()

df_male = bdat %>% 
  select(iwant, starts_with('M_'), -ends with('_N')) %>% 
  mutate(sex = 'male') %>% 
  View()

df_female = bdat %>% 
  select(iwant, starts_with('M_'), -ends with('_N')) %>% 
  mutate(sex = 'female') %>% 
  View()

df_unsexed = bdat %>% 
  select(iwant, starts_with('M_'), -ends with('_N')) %>% 
  mutate(sex = 'unsexed') %>% 
  View()

ob1 = full_join(df_male, df_female)
ob2 = full_join(ob1, df_unsexed)

dfmale %>% 
  full_join(df_female) %>% 
  full_join(df_unsexed) %>% View()

names(df_male) = names(df_male) %>%  str_remove('M_')
names(df_female) = names(df_female) %>%  str_remove('F_')
names(df_unsexed) = names(df_unsexed) %>% str_remove('Unsexed_')