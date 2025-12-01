# Logistic Regression
library(tidyverse)
library(palmerpenguins)
library(easystats)
library(ggplot2)

## Does the body mass vary significantly between penguin species?
colnames(penguins)
penguins$body_mass_g
penguins$species
penguins_raw

mod = glm(dat = penguins,
    formula = body_mass_g ~ species)
summary(mod)

# Adelie 3700 grams, Chinstrap +32.43 grams, Gentoo + 1375 grams

dat_pen = penguins

dat_pen$species = relevel(dat_pen$species, ref = 'Gentoo')
mod = glm(dat = dat_pen,
          formula = body_mass_g ~ species)
summary(mod)


dat_pen$species = factor(dat_pen$species, levels = c('Chinstrap', 'Gentoo', 'Adelie'))
mod = glm(dat = dat_pen,
          formula = body_mass_g ~ species)
summary(mod)

mod_lm = lm(dat = dat_pen,
   formula = body_mass_g ~ species)

# logistical regression
# Outcome = binary (yes or no)
View(penguins)
## Build a model to predict whether a bird is Gentoo
# Outcome is binary (0 or 1, yes or no)
mod = glm(dat = dat_pen,
    formula = Gentoo or not (y/n) ~ predictors)

#1. Outcome needs to be binary
dat_pen = dat_pen %>% 
  mutate(gentoo = case_when(species == 'Gentoo' ~ TRUE,
                            TRUE ~ FALSE))

str(dat_pen)
names(dat_pen)

mod = glm(dat = dat_pen,
          formula = gentoo ~ bill_length_mm + bill_depth_mm + flipper_length_mm + 
            body_mass_g, family = "binomial")

dat_pen$pred = predict(mod, dat_pen, type = 'response')
View(dat_pen)

dat_pen %>% 
  ggplot(aes(x = body_mass_g, y = pred, color = species)) +
  geom_point()

dat_pen %>% 
  mutate(outcome = case_when(pred > 0.75 ~ 'Gentoo', 
                             pred < 0.25 ~ 'Not Gentoo',
                             TRUE ~ 'Not sure')) %>% 
  mutate(compare = case_when(species == 'Gentoo' & outcome == 'Gentoo' ~ TRUE,
                             species != 'Gentoo' & outcome == 'Not Gentoo' ~ TRUE,
                             TRUE ~ FALSE)) %>% View()

dat_pen2 = dat_pen %>% 
  mutate(outcome = case_when(pred > 0.75 ~ 'Gentoo', 
                             pred < 0.25 ~ 'Not Gentoo',
                             TRUE ~ 'Not sure')) %>% 
  mutate(compare = case_when(species == 'Gentoo' & outcome == 'Gentoo' ~ TRUE,
                             species != 'Gentoo' & outcome == 'Not Gentoo' ~ TRUE,
                             TRUE ~ FALSE))                
table(dat_pen$species)
table(dat_pen2$outcome)
table(dat_pen2$compare)

342/(2+342)

dat_pen %>% 
  mutate(outcome = case_when(pred > 0.75 ~ 'Gentoo', 
                             pred < 0.25 ~ 'Not Gentoo',
                             TRUE ~ 'Not sure')) %>% 
  mutate(compare = case_when(species == 'Gentoo' & outcome == 'Gentoo' ~ TRUE,
                             species != 'Gentoo' & outcome == 'Not Gentoo' ~ TRUE,
                             TRUE ~ FALSE)) %>%
  pluck('compare') %>% 
  sum() / nrow(dat_pen)

## Build a model to determine if someone is getting into graduate school
read.csv(GradSchool_Admissions.csv)
