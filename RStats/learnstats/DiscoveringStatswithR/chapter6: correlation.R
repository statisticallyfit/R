library(ggplot2)
library(Hmisc); #detach("package:Hmisc")
library(ggm); #detach("package:ggm")
library(corrplot)
library(boot)
library(polycor)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstats/DiscoveringStatswithR")
getwd()

#install.packages("polycor")
#install.packages("corrplot")
##install.packages("Hmisc") # installed first 2, not last
##install.packages("ggm")
#install.packages("polycor")


toffeeData = read.delim("data/Advert.dat")
scatter = ggplot(toffeeData, aes(x=adverts, y=packets)) + geom_point(shape=19)
scatter

r = cor(toffeeData$adverts, toffeeData$packets); r
cor.test(toffeeData$adverts, toffeeData$packets)

# Testing significance of correlation coefficient
# NOTE: r sampling distribution is non-normal but the below
# transformation by Fisher makes it normal
# z_r = (1/2)* ln((1+r)/(1-r))
# SE_zr = 1/sqrt(N-3)
# statistic: z = Zr/SE_Zr
Zr = 0.5 * log((1+r)/(1-r))
n = length(toffeeData$adverts)
SE_Zr = 1/sqrt(n-3)
z = Zr/SE_Zr; z
p.value.one.tailed = 1 - pnorm(z)
p.value.two.tailed = 2*(1 - pnorm(z))

# R does this test with t-statistic:
# Tr = r*sqrt(n-2)/sqrt(1-r^2)
Tr = r*sqrt(n-2)/sqrt(1-r^2)
1 - pt(Tr, df=n-1)

# Confidence interval for Zr
confIntZr = c(Zr - 1.96*SE_Zr, Zr + 1.96*SE_Zr)
confIntZr
# convert back to r confint = (e^(2*Zr) - 1, e^(2*Zr) + 1)
rLower = (exp(2*confIntZr[1]) - 1)/(exp(2*confIntZr[1]) + 1)
rUpper = (exp(2*confIntZr[2]) - 1)/(exp(2*confIntZr[2]) + 1)
confIntR = c(rLower, rUpper); confIntR



# Correlation 
examData = read.delim("data/Exam Anxiety.dat", header=TRUE)

# use: 
# (1) everything - puts NA if something missing
# (2) all.obs - error if something missing
# (3) complete.obs - excluding cases listwise
# (4) pairwise.complete.obs - excluding cases pairwise
cor(examData$Exam, examData$Anxiety, use="complete.obs", method="pearson")
cor(examData$Exam, examData$Anxiety, use="complete.obs", method="kendall")
cor(examData$Exam, examData$Anxiety, use="pairwise.complete.obs", method="kendall")


# rcorr() does pairwise exclusion; unchangeable
rcorr(as.vector(examData$Exam), as.vector(examData$Anxiety), type="pearson")
rcorr(examData, type="pearson")

# cor.test is another way to find correlation coefficient
cor.test(examData$Exam, examData$Anxiety, alt="less", 
         method="pearson", conf.level=0.99)

# correlation matrix with cor()
cor(examData[, 1:4])



# Pearson's R

examData2 = examData[, c("Exam", "Anxiety", "Revise")]
r = cor(examData2$Exam, examData2$Anxiety, method="pearson")
r
# method 1 for cor matrix
cor(examData2)
# method 2 for cor matrix
examMatrix = as.matrix(examData2) 
rcorr(examMatrix, method="pearson") # does something other than textbook
# method 3 for cor matrix
r.mat = cor(examMatrix)
r.mat
# visualize the correlation matrix with correlogram: 
corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt=45)

# look at confidence interval of correlation for Exam and Anxiety
cor.test(examData$Anxiety, examData$Exam)$conf.int
cor.test(examData$Anxiety, examData$Revise)$conf.int
cor.test(examData$Revise, examData$Exam)$conf.int

# THis means exam performance shares 19.44% variability of anxiety
rsquared.mat = cor(examData2)^2 * 100
rsquared.mat



# Spearman's Rho: apply Pearson's R to ranked data

# put file.choose() instead if you want to click on the file
liarData = read.delim("data/The Biggest Liar.dat", header=TRUE)
# correlation matrix
cor(liarData, method="spearman")
# correlation coefficient
rho = cor(liarData$Position, liarData$Creativity, method="spearman")
rho



# Kendall's Tau: apply if many scores have same rank (and data is small)
# more accurate than spearman's method

tau = cor(liarData$Position, liarData$Creativity, method="kendall")
tau
cor.test(liarData$Position, liarData$Creativity, alt="less", method="kendall")



# Bootstrapping Correlations

# KENDALL method
bootTau = function(liarData, i) cor(liarData$Position[i], liarData$Creativity[i], use="complete.obs", method="kendall")
bootKendallInfo = boot(liarData, bootTau, 2000) # 2000 is sample size
bootKendallInfo # bias in the tau is small, stderror is based on bootstrapped samples
boot.ci(bootKendallInfo) # gives four different conf ints


# PEARSON method

bootR = function(examData2, i) cor(examData2$Exam[i], examData2$Anxiety[i], use="complete.obs", method="pearson")
bootPearsonInfo = boot(examData2, bootR, 2000)
bootPearsonInfo
boot.ci(bootPearsonInfo) 

bootR = function(examData2, i) cor(examData2$Revise[i], examData2$Anxiety[i], use="complete.obs", method="pearson")
bootPearsonInfo = boot(examData2, bootR, 2000)
bootPearsonInfo
boot.ci(bootPearsonInfo) 

bootR = function(examData2, i) cor(examData2$Exam[i], examData2$Revise[i], use="complete.obs", method="pearson")
bootPearsonInfo = boot(examData2, bootR, 2000)
bootPearsonInfo
boot.ci(bootPearsonInfo) 


# SPEARMAN method

bootRho = function(examData2, i) cor(examData2$Exam[i], examData2$Anxiety[i], use="complete.obs", method="spearman")
bootSpearmanInfo = boot(examData2, bootRho, 2000)
bootSpearmanInfo
boot.ci(bootSpearmanInfo)

bootRho = function(examData2, i) cor(examData2$Revise[i], examData2$Anxiety[i], use="complete.obs", method="spearman")
bootSpearmanInfo = boot(examData2, bootRho, 2000)
bootSpearmanInfo
boot.ci(bootSpearmanInfo)

bootRho = function(examData2, i) cor(examData2$Exam[i], examData2$Revise[i], use="complete.obs", method="spearman")
bootSpearmanInfo = boot(examData2, bootRho, 2000)
bootSpearmanInfo
boot.ci(bootSpearmanInfo)



# Point biserial correlation

catData = read.csv("data/pbcorr.csv", header=TRUE); head(catData)
r.pb = cor(catData$time, catData$gender, method="pearson"); r.pb
cor(catData$time, catData$recode) # sign has no meaning
cor.test(catData$time, catData$gender, method="pearson")
r.pb^2 # gender accounts for this % of time spent away from home

# Biserial correlation (cats were neutered so there are more than 2 categories)
# CONVERT: r.b = r.pb * sqrt(pq)/y
# y = ordinate of normal distribution at the point where there is p% of
# the area on one side and q% on the other side

# LONG WAY calculation
catFreq = table(catData$gender)
props = prop.table(catFreq); p = props[1]; q = props[2]; props
z.props = qnorm(props[1]); z.props
y = dnorm(z.props); y # y is ordinate value determined by proprtions
r.b = r.pb * sqrt(p*q)/y; r.b
# SHORT WAY calculation
polyserial(catData$time, catData$gender)
r.b

# Significance of biserial correlation
# SE.r.b = sqrt(p*q)/(y*sqrt(n))
# z.r.b = (rb - rb.bar)/SE.r.b
n = nrow(catData); n
SE.rb = sqrt(p*q)/(y*sqrt(n)); SE.rb
z.rb = r.b/SE.rb; z.rb
p.value = 2*(1 - pnorm(z.rb)); p.value

shapiro.test(catData$time) # assumptions of normality don't hold... ?




# Partial correlations

r.mat
rsquared.mat
# we are finding pure correlation between exam and anxiety
# the effect the Revise has on both Exam and Anxiety is controlled
pc = pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc; pc^2
# pcor.test(pcor object, number of control variables, sample size)
pcor.test(pc, 1, 103) #nrow(examData2) = 103

# Semi-partial correlations

# Here, we would control for the effect that Revise has on only one 
# of the other variables (either Exam or Anxiety but not both)




# Comparing Correlations (between male and female)

maleExam = subset(examData, Gender == "Male", select=c("Exam", "Anxiety"))
femaleExam = subset(examData, Gender == "Female", select=c("Exam", "Anxiety"))