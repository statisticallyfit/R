# ONE SAMPLE t TEST

daily.intake = c(5260,5470,5640,6180,6390,6515, 6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)

" H0: mu = 7725
  H0: mu != 7725 "
infoLess = t.test(daily.intake, mu=7725, alternative="less") 
infoGreater = t.test(daily.intake, mu=7725, alt="g")
infoLess$p.value + infoGreater$p.value
infoLess$statistic == infoGreater$statistic

t.test(daily.intake, mu=7725, conf.level=0.98)
t.test(daily.intake, mu=7725)


# WILCOXON TEST 
wilcox.test(daily.intake, mu=7725)
wilcox.test(daily.intake, mu=7725, alt="less", correct=FALSE)


# TWO SAMPLE t TEST
library(ISwR)
attach(energy)
energy
attributes(energy)

t.test(expend~stature)
t.test(expend~stature, var.equal=T) # assume variances are equal
