#Example 1
#built in R dataset
View(women)
#attach dataset
attach(women)
#load packages
library(RcmdrMisc)
## Loading required package: car
## Loading required package: sandwich
library(xtable)
#summary statistics of heigth
numSummary(height,
           statistics = c("mean", "sd", "quantiles", "skewness", "kurtosis"))
## mean sd skewness kurtosis 0% 25% 50% 75% 100% n
## 65 4.472136 0 -1.2 58 61.5 65 68.5 72 15
#rounded summary statistics of heigth and weight
round(numSummary(
  height,
  statistics = c("mean", "sd", "quantiles", "skewness", "kurt
                 osis")
  )$table, 2)
## mean sd skewness kurtosis 0% 25% 50% 75% 100%
## 65 4.47 0 -1.2 58 61.5 65 68.5 72
round(numSummary(
  weight,
  statistics = c("mean", "sd", "quantiles", "skewness", "kurt
                 osis")
  )$table, 2)
## mean sd skewness kurtosis 0% 25% 50% 75% 100%
## 136.73 15.5 0.28 -1.04 115 124.5 135 148 164
#export summary statistics as html
print(xtable(numSummary(
  height,
  statistics = c("mean", "sd", "quantiles", "skewness
                 ", "kurtosis")
  )$table), type = "html", file = "height.html")
#some diagrams
boxplot(height, main = "Height [inch]")
hist(height, main = "Height [inch]")
height_cat <- cut(height, c(0, 60, 70, 80, 90))
table(height_cat)
## height_cat
## (0,60] (60,70] (70,80] (80,90]
## 3 10 2 0
pie(table(height_cat), main = "Height [inch]")
barplot(table(height_cat), main = "cat. Height [inch]")
hist(height, main = "Height [inch]", c(0, 60, 70, 80, 90))
#correlation between height and weight
cor(height, weight)
## [1] 0.9954948
cov(height, weight) / (sd(height) * sd(weight))
## [1] 0.9954948
plot(height, weight, main = "Scatterplot")
lm(height ~ weight)
##
## Call:
## lm(formula = height ~ weight)
##
## Coefficients:
## (Intercept) weight
## 25.7235 0.2872
#Example 2
a <- c(157.5, 165.1, 177.8, 171.3, 162.6)
e <- c(169.5, 165.1, 177.8, 170.3, 172.6)
#KS-test without Lilliefors correction
library(nortest)
ks.test(a, "pnorm", mean = mean(a), sd = sd(a))
##
## One-sample Kolmogorov-Smirnov test
##
## data: a
## D = 0.18839, p-value = 0.9793
## alternative hypothesis: two-sided
ks.test(e, "pnorm", mean = mean(a), sd = sd(a))
##
## One-sample Kolmogorov-Smirnov test
##
## data: e
## D = 0.43123, p-value = 0.2332
## alternative hypothesis: two-sided
#KS-test with Lilliefors correction
library(nortest)
lillie.test(a)
##
## Lilliefors (Kolmogorov-Smirnov) normality test
##
## data: a
## D = 0.18839, p-value = 0.8096
lillie.test(e)
##
## Lilliefors (Kolmogorov-Smirnov) normality test
##
## data: e
## D = 0.17014, p-value = 0.9049
#F-test
var.test(a, e)
##
## F test to compare two variances
##
## data: a and e
## F = 2.8756, num df = 4, denom df = 4, p-value = 0.3308
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
## 0.2993952 27.6183078
## sample estimates:
## ratio of variances
## 2.87555
#t-test
t.test(a, e, var.equal = TRUE)
##
## Two Sample t-test
##
## data: a and e
## t = -1.0269, df = 8, p-value = 0.3345
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
## -13.631867 5.231867
## sample estimates:
## mean of x mean of y
## 166.86 171.06
boxplot(a, e, names = c("American", "European"))
#Multiple Testing
#adjusting p-values
pval <- sort(c(0.046, 0.048, 0.001, 0.0025, 0.011))
bonf <- p.adjust(pval, method = "bonferroni")
holm <- p.adjust(pval, method = "holm")
bh <- p.adjust(pval, method = "BH")
pad <-
  data.frame(bonf,
             holm,
             bh,
             row.names = c("p(1)", "p(2)", "p(3)", "p(4)", "p(5)"))
colnames(pad) <- c("Bonferroni", "Bonf.Holm", "Benj.-Hochberg")
pad
## Bonferroni Bonf.Holm Benj.-Hochberg
## p(1) 0.0050 0.005 0.00500000
## p(2) 0.0125 0.010 0.00625000
## p(3) 0.0550 0.033 0.01833333
## p(4) 0.2300 0.092 0.04800000
## p(5) 0.2400 0.092 0.04800000
#example Golub-study
library(multtest)
## Loading required package: BiocGenerics
## Loading required package: parallel
##
## Attaching package: 'BiocGenerics'
## The following objects are masked from 'package:parallel':
##
## clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
## clusterExport, clusterMap, parApply, parCapply, parLapply,
## parLapplyLB, parRapply, parSapply, parSapplyLB
## The following objects are masked from 'package:stats':
##
## IQR, mad, xtabs
## The following objects are masked from 'package:base':
##
## anyDuplicated, append, as.data.frame, cbind, colnames,
## do.call, duplicated, eval, evalq, Filter, Find, get, grep,
## grepl, intersect, is.unsorted, lapply, lengths, Map, mapply,
## match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
## Position, rank, rbind, Reduce, rownames, sapply, setdiff,
## sort, table, tapply, union, unique, unsplit
## Loading required package: Biobase
## Welcome to Bioconductor
##
## Vignettes contain introductory material; view with
## 'browseVignettes()'. To cite Bioconductor, see
## 'citation("Biobase")', and for packages 'citation("pkgname")'.
data(golub)
View(golub)
#first gene is selected
g <- 1
g_selected <- golub[g, ]
#27 patientes with ALL and 11 patients with AML
cancer <- c(rep("ALL", 27), rep("AML", 11))
#seperate data in two vectors for t-test
ALL <- g_selected[cancer == "ALL"]
AML <- g_selected[cancer == "AML"]
#Welch's t-test
t.test(ALL, AML)
##
## Welch Two Sample t-test
##
## data: ALL and AML
## t = -1.7592, df = 11.049, p-value = 0.1062
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
## -1.1078170 0.1232908
## sample estimates:
## mean of x mean of y
## -1.2715104 -0.7792473
#test of all genes with a loop
t.statistics <- vector()
p.values <- vector()
for (g in 1:nrow(golub)) {
  g_selected <- golub[g, ]
  ALL <- g_selected[cancer == "ALL"]
  AML <- g_selected[cancer == "AML"]
  t <- t.test(ALL, AML)
  t.statistics <- append(t.statistics, t$statistic)
  p.values <- append(p.values, t$p.value)
}
#unadjusted results
data_unadj <- data.frame(seq(1:nrow(golub)), p.values)
colnames(data_unadj) <- c("gene", "p_unadj")
View(data_unadj)
#adjusted results
data_adj <-
  data.frame(seq(1:nrow(golub)), p.adjust(p.values, method = "BH"))
colnames(data_adj) <- c("gene", "p_adj")
View(data_adj)
#count the number of t-tests with a p-value < 0.05
results <-
  data.frame(sum(data_unadj$p_unadj < 0.05), sum(data_adj$p_adj <
                                                   0.05))
colnames(results) <- c("unadj. p-values", "adj. p-values")
results
## unadj. p-values adj. p-values
## 1 1078 695
#select this subsets
View(data_unadj[data_unadj$p_unadj < 0.05, ])
View(data_adj[data_adj$p_adj < 0.05, ])