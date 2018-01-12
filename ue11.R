rm(list = ls())

# 20 #################################################################
people <- read.table("people.txt", header = T)
people <- people[,-2]

plot(people$Weight.kilograms., people$Bloodfat)
plot(people$Age.Years., people$Bloodfat)

# Correlation
cor(people$Weight.kilograms.,people$Age.Years.)

# LM
model <- lm(people$Bloodfat ~ people$Weight.kilograms. + people$Age.Years.)
summary(model)

# 
plot(model)

# Residuals near 0 
mean(model$residuals)

durbinWatsonTest(model$residuals)

# 21 #################################################################


# 2048 Modelle gesamt

# Index
# A1, the local selling prices, in hundreds of dollars;
# A2, the number of bathrooms;
# A3, the area of the site in thousands of square feet;
# A4, the size of the living space in thousands of square feet;
# A5, the number of garages;
# A6, the number of rooms;
# A7, the number of bedrooms;
# A8, the age in years;
# A9, construction type
# A10, architecture type
# A11, number of fire places.
# B, selling price

house <- read.table("house.txt", header = T,
                    colClasses = c(
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "numeric",
                      "factor",
                      "factor",
                      "numeric"
                    ))
str(house)


# Multicollinearity
library(corrplot)
corrplot(corr = cor(house[,c(1:8,11)]), method = "number")


model1 <- lm(house$B ~ house$A1 +
               house$A2 + 
               house$A3 +
               house$A4 +
               house$A5 +
               house$A6 +
               house$A7 +
               house$A8 +
               house$A9 +
               house$A10 +
               house$A11)

summary(model1)

library(car)
library(olsrr)
ols_vif_tol(model1)

# Stepwise Model Selection
library(MASS)
stepAIC(model1, direction = "both")

model.best <- lm(house$B ~ house$A2 + house$A4 + house$A5 + house$A7 + 
                   house$A8 + house$A9 + house$A11)
summary(model.best)
ols_vif_tol(model.best)

plot(house$A4, house$A2)
