rm(list = ls())

affairs <- read.table("http://people.stern.nyu.edu/wgreene/Text/tables/TableF22-2.txt",
                      header = T)

# Description of the independent variables in the shortened dataset:
# z1...gender(0 = female, 1 = male)
# z2...Age in years
# z3...Number of years married, 
# z4...Children(0 = no, 1 = yes)
# z8... Self rating of marriage (
#   1 = very unhappy, 2 = somewhat unhappy,
#   3 = average, 
#   4 = happier than average, 5 = very happy
# )


# EXERCISE  
# Estimate an appropriate regression model for the number of affairs in the last year

library(ggplot2)

# DATA PREPARATION
# Checking the structure of the data.frame
str(affairs)

# Factorizing categorical variables
affairs$Z1 <- factor(affairs$Z1, levels = 0:1, labels = c('female','male'))
affairs$Z4 <- factor(affairs$Z4, levels = 0:1, labels = c('no','yes'))
affairs$Z8 <- factor(affairs$Z8, levels = 1:5, labels = c('very unhappy','somewhat unhappy','average','happier than average','very happy'))

# Dummy if a person had an affair in the previous year
# Variable not necessarily essential for this task
affairs$had.affair <- ifelse(affairs$Y == 0,0,1)

# ~ 25% of the observation had at least one affair last year
sum(affairs$had.affair)/nrow(affairs)


# count of affairs on marriage rating
ggplot(affairs, aes(Y, fill = Z8)) +
  geom_histogram(binwidth=0.4, position="dodge") +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  theme_minimal()

# Most of the people had none affairs
# The maximum of affairs within last year was 12. 
# -> especially "somewhat unhappy" persons had that many affairs


# Refactorizing rate of marriage for a better interpretation of the ratings
# Base group = average rating

# Unhappy marriage
affairs$unhappy <- ifelse(affairs$Z8 == 'very unhappy'|
                            affairs$Z8 == 'somewhat unhappy',1,0)

# Happy marriage
affairs$happy <- ifelse(affairs$Z8 == 'very happy'|
                            affairs$Z8 == 'happier than average',1,0)


# Summary statistics for checking the data
# no negative nr of affairs possible
summary(affairs)



# Poisson Regression
# Average marriage is the base group
# Independent variables can be continous or categorical in the Poisson regression model
# The outcome is the logarithmic mean of success for affairs

# POISSON MODEL 
model.poisson <- glm(Y ~ Z1 + Z2 + Z3 + Z4 + happy + unhappy,
               family = poisson, data = affairs)

summary(model.poisson)
# Besides having children beeing male, age, years of marriage, and having a
# happy as well as unhappy marriage significantly influences the number of affairs
# The regression estimates are logs in a so called log-linear model so applying the e function
# is necessary for detailed interpretation

exp(coef(model.poisson))

# INTERPRETATION
# Being male has a 1.22 higher incident rate than beeing female
# An one year increase in Age has an incident rate of 0.97
# An one year incerease in marriage gives an incident rate of 1.11
# A happy marriage has a incident rate of 0.75 compared to average marriage
# In comparison to an unhappy marriage the incident rate is 2.55 
