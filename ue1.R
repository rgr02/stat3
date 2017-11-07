rm(list = ls())

library(ggplot2)
library(nortest)
library(RcmdrMisc)
source("multiplot.R")

# A.1 #########################################################################

# Time ist stetig
alc <- c(16,15,21,20,19,19)
water <- c(13,13,10,18,17,11)

data.set.1 <- as.data.frame(cbind(alc,water))

round(numSummary(
  data.set.1,
  statistics = c("mean", "sd", "quantiles", "skewness", "kurtosis")
)$table, 2)

alc <- ggplot(data = data.set.1) +
  geom_density(aes(data.set.1$alc,..density..), fill = "red", alpha = 0.3) +
  xlab("Time in Seconds") +
  ylab("Density") +
  labs(title = "Effect of Alcohol on perceptual motor abilities") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
  
water <- ggplot(data = data.set.1) +
  geom_density(aes(data.set.1$water,..density..), fill = "blue", alpha = 0.3) +
  xlab("Time in Seconds") +
  ylab("Density") +
  labs(title = "Effect of Water on perceptual motor abilities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  
multiplot(alc, water)


lillie.test(data.set.1$alc)                              # NV of Alc
lillie.test(data.set.1$water)                            # NV of Water
var.test(data.set.1$alc, data.set.1$water)               # Difference in Variances
t.test(data.set.1$alc, data.set.1$water, var.equal = T)  # Mittelwerttest

# Weiterer Test für Varianz -> Levin Test
# 


###############################################################################
# A.2 #########################################################################
###############################################################################

# Satisfaction Score is ordinal

# old <- factor(c(59,48,52,49,46,16), levels = 0:60, ordered = T)
# young <- factor(c(34,22,15,27,37,41), levels = 0:60, ordered = T)


old <-(c(59,48,52,49,46,16))
young <-(c(34,22,15,27,37,41))


data.set.2 <- as.data.frame(cbind(young, old))
# str(data.set.2)

round(numSummary(
  data.set.2,
  statistics = c("median", "quantiles")
)$table, 2)

ggplot() +
  geom_bar(aes(young), fill = "yellow", alpha = 0.3) +
  geom_bar(aes(old), fill = "green", alpha = 0.3) +
  xlab("Lebenszufriedenheit") +
  ylab("Häufigkeit") +
  labs(title = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Ohne Bindung
# cor(old, young, method = "spearman")

cor.test(old, young, method = "spearman")

# Wilcox test oder Mann-Whitney-U Test
wilcox.test(old, young)

t.test(old, young)

par(mfrow = c(1,2))
pie(table(young))
title("Young")
pie(table(old))
title("Old")


