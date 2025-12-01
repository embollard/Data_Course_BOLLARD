getwd()

## 1. Read 'cleaned_bird_data.csv'
read.csv('Data/cleaned_bird_data.csv')
## 2. Calcuate avg of egg size
names(df_bird)
mean(df_bird$Egg_mass, na.rm = T)
## 3. Save birds with egg size > avg
large_egg = df_bird %>% 
  filter(Egg_mass > 21.8)
## 4.Save into a .csv in your laptop
write.csv(large_egg, 'large_egg.csv, row.names = F')
##5. Read this csv file back to R again
df_from_csv = read.csv('large_egg.csv')
pwd

head(penguins)


##Make graph avg weight by penguin species
penguins %>% 
  ggplot(aes(x = species, 
             y = body_mass)) + 
  geom_col()
##adding error bar
penguins %>% 
  ggplot(aes(x = species, 
             y = body_mass)) +
  geom_bar(stat = 'identity')
geom_errorbar(aes(ymin = mena() - sd(),
                  ymax = mena() + sd()))

penguins %>% 
  filter(!is.na(body_mass)) %>% 
  group_by(species) %>% 
  summarise(avg_weight = mean(body_mass), 
            sd_weight = sd(body_mass)) %>% 
  ggplot(aes(x = species, 
             y = avg_weight, 
             fill = 'sex')) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = avg_weight - sd_weight, 
                    ymax = avg_weight + sd_weight), 
                    position = position_dodge(width = 0.9),
                    width = 0.3) +
                  scale_y_continuous(expand = c(0,0))

##Make a histogram 
penguins %>% 
  ggplot(aes(x = 'flipper_len',
             y = "body_mass")) +
  geom_histogram(stat = 'identity')
head(penguins)


##plot 
penguins %>% 
  mutate(flipper_group = case_when(flipper_len > 205 ~ 'Big flipper'))
  ggplot(aes(x = 'flipper_len',
             y = "body_mass")) +
  geom_histogram(stat = 'identity')
  
  ##boxplot to show penguin weight across years.
  penguins %>% 
    ggplot(aes(x = factor(year),
               y = body_mass,
               color = year)) +
    geom_boxplot() +
    geom_density()
  
unique(penguins$year)
str(penguins)
  
  head(penguins)
