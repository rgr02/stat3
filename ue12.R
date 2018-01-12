rm(list = ls())

# 22 ################################################################

# Kilogramm
Weight <- c(280,340,430,480,550,580,590,600,590,600)

# Month
x <- c(8,12,24,36,48,60,72,84,96,108)

data <- as.data.frame(cbind(Weight, x))

A <- max(data$Weight)
W <- min(data$Weight)
k <- 0.037


# Brody Growth Function
brodyGC <- function(x,A,W,k) (A - (A-W)*exp(-k*(x-min(x))))

with(data, plot(x, Weight))
curve(brodyGC(x,A,W,k),add = T)

# Nonlinear
library(nls2)

nonlinear <- nls2(Weight ~ brodyGC(x,A,W,k),data = data,
                  start = list(A = max(Weight),
                  W = min(Weight),
                  k = 0.037))
library(nlshelper)
library(nlstools)
library(broom)
library(car)
library(minpack.lm)


# Levenberg - Marquardt
nonlinear2 <- nlsLM(Weight ~ brodyGC(x,A,W,k),data = data,
                    start = list(A = max(Weight),
                    W = min(Weight),
                    k = 0.037))

summary(nonlinear)
summary(nonlinear2)

tidy(nonlinear,conf.int=TRUE)
tidy(nonlinear2,conf.int=TRUE)

plot_nls(nonlinear)

# 23 ########################################################################

E.Consumpt <-c(1290, 1350,1470,1600,1710,1840,1980,2230,2400,2930)
Area <- c(1182,1172,1264,1493,1571,1711,1804,1840,1956,1954)


data <- as.data.frame(cbind(E.Consumpt, Area))
plot(data$Area, data$E.Consumpt)

aic <- c()

model1 <- lm(E.Consumpt ~ Area, data = data)
summary(model1)
aic <- c(aic,AIC(model1))

model2 <- lm(E.Consumpt ~ Area + I(Area^2), data = data)
summary(model2)
aic <- c(aic,AIC(model2))

model3 <- lm(E.Consumpt ~ Area + I(Area^2) + I(Area^3), data = data)
summary(model3)
aic <- c(aic,AIC(model3))

model4 <- lm(E.Consumpt ~ Area + I(Area^2) + I(Area^3) + I(Area^4), data = data)
summary(model4)
aic <- c(aic,AIC(model4))
aic

library(olsrr)
ols_diagnostic_panel(model = model3)
ols_diagnostic_panel(model = model2)

plot(data$Area, log(data$E.Consumpt))