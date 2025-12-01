## 1. Create a dataframe using mtcars, for map >20
getwd()
df_cars = mtcars

## and cyl equal to 6
df_20 = df_cars[df_cars$mpg > 20, ]
df_20_6 = df_20[df_20$cyl == 6, ]

df_20_6
## 2. in the data frame add a new column mpg x cyl
new_col = df_20_6$mpg * df_20_6$cyl

## 3. write a for loop to print out each row
df_20_6$mpgcyl = new_col

## read/load data
read.csv()
write.csv(mtcars.csv'))

library(tidyverse)
control shift m = %>% 

## 1. read 'cleaned_bird_data.csv'
## 2. calculate avg of egg size
## 3. save birds with egg size > avg
## 4. Save into a .csv in your laptop
## 5. Read this csv file back to R again

read.csv('cleaned_bird_data.csv')
read.csv('Data/cleaned_bird_data.csv')
bird_data = 'Data/cleaned_bird_data.csv'

