rm(list = ls())


library(nortest)
# Aufgabe 9 ###################################################


# In a clinical experiment the effect of the actual time of 
# measurement on the resting heart rate was examined. 
# Five patients who suffered from a mitral valve prolapse 
# were examined in the morning, at midday and in the evening. 
# The research question is, if the heart rates vary over time.


Morning <- c(80,77,71,72,75)
Midday <- c(84,74,81,80,79)
Evening <- c(76,76,74,73,74)

# Testing for NV
lillie.test(Morning)
lillie.test(Midday)
lillie.test(Evening)


patId <- factor(c(seq(1:5),seq(1:5), seq(1:5)))

time <- as.factor(
  c(rep("Morning", length(Morning)),
    rep("Midday", length(Midday)),
    rep("Evening", length(Evening))))

heart.rate <- data.frame(rate = c(Morning, Midday, Evening),
                         time,patId)

result <- aov(heart.rate$rate ~ heart.rate$time + Error(heart.rate$patId), data = heart.rate)
summary(result)

boxplot(heart.rate$rate ~ heart.rate$time, col = c(2,3,4))
abline(a = mean(heart.rate$rate), b = 0)

tukey <- lm(rate ~ patId + time, data = heart.rate)
TukeyHSD(aov(tukey), "time")
