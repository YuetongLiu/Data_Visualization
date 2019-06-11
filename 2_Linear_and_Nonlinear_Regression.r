library(dplyr)  # for manipulating data frames 
library(ggplot2)  # for data viz
library(here)  # for simplifying folder references 
library(readr)  # reading and writing data files 
library(GGally)  # extension to ggplot2
library(ISLR)  # from the book Intro to Stat Learning with R 
library(broom)  # for saving model outputs as dataframes 
library(janitor)  # optional; for cleaning up dataframes 

p1.group.means <- mtcars %>%
  
  # let's recode cyl as a discrete variable: 
  mutate(cyl = factor(cyl, 
                      levels = c(4, 6, 8))) %>% 
  
  # now start using gpplot functions: 
  ggplot(aes(x = disp,  # specify x and y axis
             y = mpg)) + 
  
  geom_point(aes(colour = cyl, 
                 size = hp)) + 
  
  # examine three different ways of summarizing behaviour within
  # each level of cyl: 
  
  # mean by group: 
  stat_smooth(aes(group = cyl,
                  colour = cyl), 
              method = "lm", 
              formula = y ~ 1) + 
  
  theme_classic()

# print: 
p1.group.means

#1.Regression on categorical variable
df1.mtcars <- mtcars %>% 
  mutate(cyl = as.factor(cyl))

m1.mpg.vs.cyl <- lm(mpg ~ cyl - 1, data = df1.mtcars)

summary(m1.mpg.vs.cyl)

#1.1 clean up and create a nice dataframe
#1 row per coefficient 
broom::tidy(m1.mpg.vs.cyl) %>% 
  head
#1 row per model; very useful when comparing lots of models
broom::glance(m1.mpg.vs.cyl) %>% 
  head
#1 row per observation
broom::augment(m1.mpg.vs.cyl) %>% 
  head

#2 Averages by group, after accounting for weight
m2.include.wt <- lm(mpg ~ cyl + wt, data = df1.mtcars)

summary(m2.include.wt)

#2.1 Diagnostic plots
# display 4 plots in a 2 by 2 grid: 
par(mfrow = c(2, 2))

plot(m2.include.wt)
#scale-location -> similar to residual
#leverage ->more cooks distance, more influencial outliers.

#3 prediction
predict(m2.include.wt)

augment(m2.include.wt) %>% 
  select(mpg, cyl, wt, .fitted) %>% 
  head(15) 

#3.1 prediction in graph
df1.mtcars %>% 
  mutate(pred.mpg = predict(m2.include.wt)) %>% 
  
  ggplot(aes(x = wt, 
             y = pred.mpg)) + 
  
  geom_point(aes(col = cyl))

#3.2 new data using prediction
# create new fake data: 
df2.mtcars.fake <- data.frame(cyl = as.factor(c(4, 6, 8)), 
                              wt = c(4, 2, 2))
df2.mtcars.fake 

# note the "newdata" argument in predict() below
df2.mtcars.fake <- 
  df2.mtcars.fake %>% 
  mutate(pred.mpg = predict(m2.include.wt, 
                            newdata = df2.mtcars.fake)) 

# first plot the points that were originally in the data:  
df1.mtcars %>% 
  mutate(pred.mpg = predict(m2.include.wt)) %>% 
  
  ggplot(aes(x = wt, 
             y = pred.mpg)) + 
  
  geom_point(aes(col = cyl)) + 
  
  # then add the new points. Increase size to make them distinguishable: 
  geom_point(data = df2.mtcars.fake, 
             aes(x = wt, 
                 y = pred.mpg, 
                 col = cyl), 
             size = 5)
#3.3 Plotting actuals vs predicted values to assess model performance
augment(m2.include.wt) %>% 
  
  ggplot(aes(x = .fitted, 
             y = mpg)) + 
  
  geom_point(aes(col = cyl)) + 
  
  # add 45 degree line: 
  geom_abline(slope = 1, 
              intercept = 0, 
              col = "blue") + 
  
  # make sure axes have same scale: 
  scale_x_continuous(limits = c(10, 35)) + 
  scale_y_continuous(limits = c(10, 35)) + 
  
  labs(title = "Actuals vs predictions", 
       subtitle = "Points above the line are being under-estimated \nPoints below the line are being over-estimated")

