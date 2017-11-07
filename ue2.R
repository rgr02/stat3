rm(list = ls())


# Bsp 3 ##################################################################################
 
# Suppose someone performed 10 tests (e.g. tests of the association between a specific outcome
# and 10 different potential prognostic factors on the same data set) and obtained the p-values
# below: 0.0140, 0.2960, 0.9530, 0.0031, 0.1050, 0.6410, 0.7810, 0.9010, 0.0053, 0.4500
# Use two appropriate methods to control for a global type I error of 5%.


p.values <- c(0.0140, 0.2960, 0.9530, 0.0031, 0.1050, 0.6410, 0.7810,
              0.9010, 0.0053, 0.4500)



# Global alpha value
a.global <- round(1-(1-0.005)^10,4)

# FWER - At least 1 false positive by chance

# Bonferroni
a.local <- rep(round(0.05/10,4),10)

# how to do the eqivalent adjustments
p.values <= a.local

# Bonferroni-Holm
p.values <- sort(p.values)

for(i in 1:10){
  a.local <- 0.05/(10-i+1)
  if(p.values[i] <= a.local){
    print(p.values[i])
  }else{
    print("Signifikanzniveau überschritten")
    break
  }
}

# Benjamini Hochberg
# Benjamini Hochberg geschickteste Variante weil die Power größer ist.

# Bsp 4 ##################################################################################

nr.test <- 6*7 # Risikofaktoren x Disease
by.chance <- 42*0.05
global.alpha <- 1-(1-0.05)^42

# adjustierte Residuen vergleichen mit 1.96


# Bonferroni-Holm