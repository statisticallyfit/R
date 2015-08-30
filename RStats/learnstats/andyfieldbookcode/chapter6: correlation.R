library(ggplot2)
library(Hmisc); #detach("package:Hmisc")
library(ggm); #detach("package:ggm")

##install.packages("Hmisc") # installed first 2, not last
##install.packages("ggm")
#install.packages("polycor")



toffeeData = read.delim("data/Advert.dat")
scatter = ggplot(toffeeData, aes(x=adverts, y=packets)) + geom_point(shape=19)
scatter

r = cor(toffeeData$adverts, toffeeData$packets)

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
rcorr(examData$Exam, examData$Anxiety, type="pearson")
rcorr(examData, type="pearson")


cor.test(examData$Exam, examData$Anxiety, alt="less", 
         method="pearson", conf.level=0.99)





# Analyse the data
examData2 = examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2)
