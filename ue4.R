rm(list = ls())

library(RcmdrMisc)
library(multcomp)
library(DescTools)

# Aufgabe 7 ################################################################

const.reward <- c(12,13,11,12,12)
freq.reward <- c(9,10,9,13,14)
infreq.reward <- c(15,16,17,16,16)
no.reward <- c(17,18,12,18,20)


dv <- c(const.reward, freq.reward, infreq.reward, no.reward)
iv <- as.factor(
  c(rep("Constant", length(const.reward)),
    rep("Frequent", length(freq.reward)),
    rep("Infrequent", length(infreq.reward)),
    rep("No Reward", length(no.reward)))
)

data <- data.frame(dv,iv)

#summary of the data
numSummary(dv,groups=iv,statistics=c("mean","sd"))$table
# 
# Statistic
# Group        mean        sd
# Constant     12 0.7071068
# Frequent     11 2.3452079
# Infrequent   16 0.7071068
# No Reward    17 3.0000000


boxplot(dv~iv,main="Number of trials")
abline(h = mean(c(12,11,16,17)), col = "red")

result <- aov(dv ~ iv)
summary(result)

#             Df    Sum Sq Mean Sq  F value   Pr(>F)    
# iv           3    130     43.33   11.18     0.000333 ***
# Residuals   16     62     3.88                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Mindestens 2 means sind unterschiedlich!

print(model.tables(result,"means"))

# Tables of means
# Grand mean
# 
# 14 
# 
# iv 
# iv
# Constant   Frequent Infrequent  No Reward 
# 12         11         16         17 

# Kontraste
k1 <- c(1, (-1/3), (-1/3), (-1/3))
k2 <- c(0, 1, -0.5, -0.5)
k3 <- c(0, 0 , 1, -1)

# Kontraste sind orthogonal :)
sum(k1*k2*k3)


summary(glht(result, linfct = mcp(iv = k1)))

# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: User-defined Contrasts
# 
# 
# Fit: aov(formula = dv ~ iv)
# 
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)  
# 1 == 0   -2.667      1.017  -2.623   0.0184 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)

summary(glht(result, linfct = mcp(iv = k2)))

# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: User-defined Contrasts
# 
# 
# Fit: aov(formula = dv ~ iv)
# 
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)    
# 1 == 0   -5.500      1.078  -5.101 0.000107 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Adjusted p values reported -- single-step method)

summary(glht(result, linfct = mcp(iv = k3)))

# Simultaneous Tests for General Linear Hypotheses
# 
# Multiple Comparisons of Means: User-defined Contrasts
# 
# 
# Fit: aov(formula = dv ~ iv)
# 
# Linear Hypotheses:
#   Estimate Std. Error t value Pr(>|t|)
# 1 == 0   -1.000      1.245  -0.803    0.434
# (Adjusted p values reported -- single-step method)

# Pairwise comparison if contrasts wouldn't be orthogonal
ScheffeTest(result)

# Aufgabe 8 ############################################################

Standard.medication <- c(145,155,170,160,155)
Medication.A <- c(109,110,120,130,140)
Medication.B <- c(142,143,141,142,142)
Medication.C <- c(127,138,120,121,122)


dv <- c(Standard.medication,
        Medication.A,
        Medication.B,
        Medication.C)

# recode Medication because A is taken as default instead of standard
iv <- as.factor(
  c(rep("A", length(Standard.medication)),
    rep("B", length(Medication.A)),
    rep("C", length(Medication.B)),
    rep("D", length(Medication.C)))
)

#data <- data.frame(dv,iv)

summary(aov(data$dv~data$iv))

#             Df Sum Sq   Mean Sq   F value   Pr(>F)    
# data$iv      3   3927   1308.9   16.65     3.55e-05 ***
# Residuals   16   1258    78.6                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

DunnettTest(dv~iv)

# Dunnett's test for comparing several treatments with a control :  
#     95% family-wise confidence level
# 
# $A
# diff      lwr.ci     upr.ci     pval    
# B-A -35.2 -49.73944 -20.6605633   4e-05 ***
# C-A -15.0 -29.53944  -0.4605633 0.04232 *  
# D-A -31.4 -45.93944 -16.8605633 0.00015 ***
# 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

