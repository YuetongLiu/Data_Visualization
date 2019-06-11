library(dplyr)
library(here) 
library(readr)
library(GGally)
library(gapminder)
library(fpp)
library(ggplot2)

#1.1 Built-in datasets
str(mtcars)
summary(mtcars)
head(mtcars) %>% select(mpg, cyl, hp, disp)

#1.2 Side-by-side analysis
df1.mtcars.modified <- mtcars %>% 
  # Notes on notation: 
  # "x <- 10" means "save the number 10 with object name x"
  # "%>%" translates as "then". That is, first do x %>% do y
  
  # select certain COLUMNS
  select(cyl, 
         mpg, 
         disp) %>% 
  
  # filter out certain ROWS
  filter(mpg <= 30) %>%  # let's say these are outliers 
  
  rename(cylinders = cyl, 
         miles.per.gallon = mpg, 
         displacement = disp) %>% 
  
  # let's say one of the entries of mpg was a known data error: 
  mutate(miles.per.gallon = case_when(
    miles.per.gallon == 15 ~ 15.5,  # "~" is like the "then" statement in SQL
    TRUE ~ miles.per.gallon
  )) %>% 
  
  # Wait, what kind of savages use units like miles and gallons? 
  # let's create a new column with proper civilized units: 
  mutate(kilometres.per.litre = (1.609*miles.per.gallon)/3.785) %>% 
  group_by(cylinders) %>% 
  
  summarise(avg.kpl = mean(kilometres.per.litre) %>% round(1), 
            avg.disp = mean(displacement) %>% round(1))

df1.mtcars.modified 
          
#2 Data visualization with ggplot
#2.1.1 overall.mean
p1.overall.mean <- mtcars %>% 
  
  # let's add in the kpl column again: 
  mutate(kpl = (1.609*mpg)/3.785) %>%
  
  # let's recode cyl as a discrete variable (aka "factor"): 
  mutate(cyl = factor(cyl, 
                      levels = c(4, 6, 8))) %>% 
  
  # now start using gpplot functions: 
  ggplot(aes(x = disp,  # specify x and y axis
             y = kpl)) + 
  
  # geom_point creates a scatterpolot
  geom_point(aes(colour = cyl, 
                 size = hp)) + 
  
  # overall mean: 
  stat_smooth(method = "lm", 
              formula = y ~ 1) + 
  
  theme_classic()


# print: 
p1.overall.mean

#2.1.2 group.means
p2.group.means <- mtcars %>% 
  
  # let's add in the kpl column again: 
  mutate(kpl = (1.609*mpg)/3.785) %>%
  
  # let's recode cyl as a discrete variable: 
  mutate(cyl = factor(cyl, 
                      levels = c(4, 6, 8))) %>% 
  
  # now start using gpplot functions: 
  ggplot(aes(x = disp,  # specify x and y axis
             y = kpl)) + 
  
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
p2.group.means

#2.1.3 group.trends
p3.group.trends <- mtcars %>% 
  
  # let's add in the kpl column again: 
  mutate(kpl = (1.609*mpg)/3.785) %>%
  
  # let's recode cyl as a discrete variable: 
  mutate(cyl = factor(cyl, 
                      levels = c(4, 6, 8))) %>% 
  
  # now start using gpplot functions: 
  ggplot(aes(x = disp,  # specify x and y axis
             y = kpl)) + 
  
  geom_point(aes(colour = cyl, 
                 size = hp)) + 
  
  # examine three different ways of summarizing behaviour within
  # each level of cyl: 
  
  # mean by group: 
  stat_smooth(aes(group = cyl,
                  colour = cyl), 
              method = "lm") + 
  
  theme_classic()

# print: 
p3.group.trends

#2.1.4 overall.trend
p4.overall.trend <- mtcars %>% 
  
  # let's add in the kpl column again: 
  mutate(kpl = (1.609*mpg)/3.785) %>%
  
  # let's recode cyl as a discrete variable: 
  mutate(cyl = factor(cyl, 
                      levels = c(4, 6, 8))) %>% 
  
  # now start using gpplot functions: 
  ggplot(aes(x = disp,  # specify x and y axis
             y = kpl)) + 
  
  geom_point(aes(colour = cyl, 
                 size = hp)) + 
  
  # examine three different ways of summarizing behaviour within
  # each level of cyl: 
  
  # mean by group: 
  #??
  #stat_smooth(method = "lm") +  # also try "lm"
  stat_smooth()#method = "loess" -> non-para; K-nearest
  theme_classic()

# print: 
p4.overall.trend

#2.2 boxplot
p5.box <- mtcars %>% 
  
  # let's add in the kpl column again: 
  mutate(kpl = (1.609*mpg)/3.785) %>%
  
  # let's recode cyl as a discrete variable: 
  mutate(cyl = factor(cyl, 
                      levels = c(4, 6, 8))) %>% 
  
  # now start using gpplot functions: 
  ggplot(aes(x = cyl,  # specify x and y axis
             y = kpl)) + 
  
  geom_boxplot() + 
  
  # by default, boxplot shows only median, not mean
  # we'll add in the mean here: 
  stat_summary(fun.y = mean, 
               geom = "point", 
               colour = "firebrick") + 
  
  theme_classic()

# print: 
p5.box

#??
#2.3 Summary of all important variables in the dataset
p6.pairs <- mtcars %>% 
  select(mpg, 
         cyl, 
         hp, 
         disp) %>% 
  
  mutate(cyl = as.factor(cyl)) %>% 
  
  ggpairs()

# print: 
p6.pairs


#3. models for data
str(gapminder)

p7.gapminder.pairs <- gapminder %>% 
  select(-c(country)) %>% 
  ggpairs()

p7.gapminder.pairs

#3.1 origin graph
p8.gapminder.gdppercap <- gapminder %>% 
  ggplot(aes(x=year, 
             y=gdpPercap)) + 
  geom_jitter(aes(colour = continent) ,
              alpha = 0.2) + 
  stat_smooth(aes(group = continent, 
                  colour = continent)) + 
  # scale_y_log10() + 
  
  theme_classic()

p8.gapminder.gdppercap

# 3.2 spread out
p9.gdppercap.log <- p8.gapminder.gdppercap + 
  scale_y_log10()

p9.gdppercap.log

#3.3 split graph
p10.life.gdp <- gapminder %>% 
  ggplot(aes(x = log(gdpPercap), 
             y = lifeExp)) + 
  
  geom_point(aes(colour = year),
             #alpha shows near cluster
             alpha = 0.5) + 
  
  
  facet_wrap(~continent) + 
  
  geom_smooth(method = "lm")

p10.life.gdp