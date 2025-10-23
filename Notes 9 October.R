##read'/Users/Angie/Desktop/Data_Course_BOLLARD/Data/DatasaurusDozen.tsv
getwd()
dat = read_tsv('Data/DatasaurusDozen.tsv')
## Examine the data
head(dat)
df = read.delim('Data/DatasaurusDozen.tsv')
dim(df)
unique(df$dataset)
## Make a cool plot
df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x),
            max_x = max(x),
            min_x = min(x),
            mean_y = mean(y),
            max_y = max(y),
            min_y = min(y))

df %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_point()


df %>% 
  ggplot(aes(x = x,
             fill = dataset)) +
  geom_density(alpha = 0.5)

df %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_point() +
  facet_wrap(~ dataset)

## install 'ggally'
install.packages("GGally")
library(GGally)

GGally::ggpairs(df)

penguins$sex

penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g,
             color = sex)) +
  geom_point(alpha = 0.7, size = 4) +
  facet_wrap(~species) +
  scale_color_viridis_c(end = 0.2) +
  labs(x = "Bill Depth (mm)",
       y = "Body Mass (g)", 
       color = Sex)

install.packages(gapminder)
view(gapminder)
unique(gapminder$country)
unique(gapminder$year)
range(gapminder$year)

## Explore the data
unique(gapminder$lifeExp)
range(gapminder$lifeExp)
unique(gapminder$gdpPercap)
range(gapminder$gdpPercap)
unique(gapminder$pop)
range(gapminder$pop)
unique(gapminder$country)
range(gapminder$country)
view(gapminder$country)

gapminder$new_col = gapminder$country * gapminder$lifeExp
gapminder$new_col2 = gapminder$country / gapminder$lifeExp

gapminder %>% 
  ggplot(aes(x = country,
             y = lifeExp)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~year) +
  labs(x = "Country",
       y = "Life Expectancy")

##Make a good figure and save to your local directory
df = gapminder
ggpairs(df)

## average life expectancy by continent
df %>% 
  group_by(country) %>% 
  summarise(avg_lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x = year,
             y = avg_lifeExp,
             color = country)) +
  geom_point() +
  facet_wrap(~country) 

p1 = df %>% 
  ggplot(aes(x = year,
             y = lifeExp,
             color = continent)) +
  geom_point() +
  facet_wrap(~ continent) 

install.packages('gganimate')
library(gganimate)

ani = p1 + transition_time(year) +
  labs(title = 'Year: {frame_time}')



ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

install.packages("gifski")
install.packages("av")

df %>% 
  ggplot(aes(x = gdpPercap, 
             y = lifeExp,
             color = continent)) +
  labs(x = 'GDP', y = "Life Expectancy") +
  
##Make a cool animated plot
  df %>% 
  group_by(country) %>% 
  summarise(avg_lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x = country,
             y = avg_lifeExp,
             color = country)) +
  geom_point() +
  facet_wrap(~country)  
## Label the country
labs(x = 'Country', y = "Life Expectancy") 
## save to your local directory
ggsave()
anim_save()

## Install leaflet and ggmap
install.packages('leaflet')
library('leaflet')

install.packages('ggmap')
library('ggmap')

getwd()
setwd('C:/Users/Angie/Desktop/Data_Course_BOLLARD/Data')

##read wide_income_rent.csv
df= read.csv('wide_income_rent.csv')
head(df)
colnames(df)
col(df)
row(df)
t.data.frame(x = )

pivot_longer()

