rm(list = ls())

library(car)

# Aufgabe 10 ######################################################

# In a clinical experiment five patients who suffered from high 
# blood pressure were examined in the morning, at midday and in 
# the evening. The research question is, if the resting heart rates 
# vary over time.
# 
# Test for a global difference over time (type I error = 5%). 
# If a significant global difference exists use an appropriate method 
# to conduct multiple pairwise comparisons.

Morning <- c(80,77,70,72,75)
Midday <- c(79,80,81,80,79)
Evening <- c(76,76,74,73,74)

# Maulchy Test
rate <- cbind(Morning,Midday,Evening)
rate.model <- lm(rate ~ 1)
mauchly.test(rate.model) # Sphericity not met !

# definition of the 3 timepoints of measurement
timepoints <- as.factor(c(1, 2, 3))
timeframe <- data.frame(timepoints)
#combining this with the time for repeated measures ANOVA
analysis <- Anova(rate.model, idata = timeframe, idesign = ~timepoints)
summary(analysis)


# Prozedur von Aufgabe 9 für Tukey HSD
patId <- factor(c(seq(1:5),seq(1:5), seq(1:5)))

time <- as.factor(
  c(rep("Morning", length(Morning)),
    rep("Midday", length(Midday)),
    rep("Evening", length(Evening))))

heart.rate <- data.frame(rate = c(Morning, Midday, Evening),
                         time,patId)

tukey <- lm(rate ~ patId + time, data = heart.rate)
TukeyHSD(aov(tukey), "time")

# Aufgabe 11 ######################################################






