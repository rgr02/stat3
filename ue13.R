rm(list = ls())


# 24 ######################################################################

library(minpack.lm)
library(car)
str(UN)

data <- UN
data <- na.omit(data)
summary(data)

plot(data$gdp,data$infant.mortality)

data$l_gdp <- log(data$gdp)
data$l_infant.mortality <- log(data$infant.mortality)

plot(data$l_gdp, data$l_infant.mortality)

# log
model1 <- lm(l_infant.mortality ~ l_gdp , data = data)
summary(model1)

# Wenn GDP um 1% Punkt steigt vermindert sich die 
# Säuglingssterblichkeit um -0.49% Punkte auf 1000
# Lebendgeburten

# segmented log log
model2 <- lm(infant.mortality ~ gdp + I((gdp -2000)*(gdp > 2000)),
             data = data)

summary(model2)

# poly
model3 <- lm(l_infant.mortality ~ l_gdp + I(l_gdp^2) + I(l_gdp^3),
             data = data)
summary(model3)

# exponential
mortality <- function(x,b0,b1) b0*(exp(b1*x)) 

nls_modLM <- nlsLM(infant.mortality ~ mortality(gdp,b0,b1),
                   start=list(b0=0, b1=0),
                   data = data)

summary(nls_modLM)

plot(data$gdp, data$infant.mortality)
p<-coef(nls_modLM)
curve(mortality(x, b0=p[1], b1=p[2]), add=TRUE, col = 'red')

# 25 #####################################################################

affairs <- read.table("http://people.stern.nyu.edu/wgreene/Text/tables/TableF22-2.txt",
                   header = T,
                   colClasses = c('factor','numeric','numeric',
                                  'factor','factor'))

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


str(affairs)
summary(affairs)

affairs$had.affair <- ifelse(affairs$Y == 0,0,1)
sum(affairs$had.affair)/nrow(affairs)

# dependent variable is not normal distributed
hist(affairs$Y)

# Faktorvariablen auf Dummies umkodieren!
model <- glm(had.affair ~ Z1+Z2+Z3+Z4+Z8,
             family = 'binomial',
             data = affairs)
summary(model)

# Odds Ratio
exp(coef(model))

# men increases odds by 46%
# age in years decreses odds by ~ 4%
# one year in marriage increases odds by 0.8%
# having children increases odds by 43% ??????
# happier marriage decreases odds by 38%


