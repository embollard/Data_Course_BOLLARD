install.packages("easystats")
library(easystats)
## build a model that predicts cty as a function of displ
## "mpg" dataset
mod = glm(data = mpg,
          formula = cty ~ displ)
## using displ to predict cty
mod = glm(data = mpg,
          formula = cty ~ displ) %>% 
summary()
mpg$displ

str(mod)
mod$formula
mod$model
mod$coefficients
mpg$displ

mpg%>% 
  ggplot(aes(x = displ, y = cty)) +
  geom_point() +
  geom_smooth(method = 'glm')

mpg$pred1 = predict(mod, mpg)

plot(mod$fitted.values, mpg$pred1)

predict(mod, data.frame(displ = 1:20))

range(mpg$displ)

report(mod)
performance(mod)
check_model(mod)
View(mpg)

## build your own models to predict cty mpg
mod1 = glm(data = mpg,
          formula = cty ~ cyl)
summary(mod1)

mod2 = glm(data = mpg,
          formula = cty ~ class)
summary(mod2)

mod3 = glm(data = mpg,
           formula = cty ~ trans)
summary(mod3)

mod4 = glm(data = mpg,
           formula = cty ~ year)
summary(mod4)

performance(mod2)
performance(mod3)
performance(mod4)
compare_performance(mod, mod1, mod2, mod3, mod4, mod5) %>% plot()
mod5 = glm(data = mpg,
           formula = cty ~ class + cyl)

## Additive effects
mod6 = glm(data = mpg,
           formula = cty ~ class * cyl)
summary(mod6)

## Make a prediction of cty based on my models and save
mpg$pred1 = predict(mod, mpg)
mpg$pred2 = predict(mod1, mpg)
mpg$pred3 = predict(mod2, mpg)
mpg$pred4 = predict(mod3, mpg)
mpg$pred5 = predict(mod4, mpg)
mpg$pred6 = predict(mod5, mpg)
mpg$pred7 = predict(mod6, mpg)

mpg %>% 
  ggplot(aes(x = displ, y = pred1, color = , group = ))


## This would work well for Exam 2!!!!!! ####
mpg %>% 
  pivot_longer(starts_with('pred')) %>% 
  ggplot(aes(x = displ, y = cty, color = factor(cyl))) +
  geom_point() +
  geom_point(aes(y = value), color = 'black') +
  facet_wrap(~ name)

library(MASS)
mod_max = glm(data = mpg, formula = cty ~ .^2) +
  step_max = stepAIC(mod_max)
step$formula