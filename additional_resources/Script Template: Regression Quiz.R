rm(list = ls()) # Q1: What does this do? Answer: 

library(tidyverse) # Q2: Is this the right place to put your libraries? Answer: 

data(swiss) # this is a preloaded dataset in R (Fertility across Swiss provinces in 1888). Just run this line to have it loaded into your environment
# Q3: What usually goes here instead if this is data you downloaded or for an in class task?

swiss # run this line to see what is in the data

# Q4: Run this regression and interpret the two coefficients that come out of it
# Intercept/b0:
# Slope/b1 for Catholic: 
lm(Fertility ~ Catholic, data = swiss)

# Q5: Run this regression and interpret the three coefficients that come out of it
lm(Fertility ~ Catholic + Education, data = swiss)

# Intercept/b0:
# Slope/b1 for Catholic:
# Slope/b2 for Education: 