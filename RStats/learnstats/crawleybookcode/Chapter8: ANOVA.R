" SITUATIONS: 

 (1) if there is one factor with 3 or more levels, use one-way ANOVA
 (2) if 1 factor with 2 levels, use t-test (F = t^2)
 (3) if 2 or more factors, use two or three way ANOVA  
 (4) factorial design: used if there is replication at each level 
in a multi-way ANOVA to test if the response to one factor depends 
on the level of another factor. "

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstats/crawleybookcode")

# (1) ONE-WAY ANOVA
oneway <- read.csv("data/oneway.csv")
attach(oneway)
oneway

plot(1:20, ozone, ylim=c(0,8), ylab="y", xlab="order", pch=19, col="red")
abline(h=mean(ozone), col="blue")
# segments
for(i in 1:20)
  lines(c(i, i), c(mean(ozone), ozone[i]), col="green")

# look at departure of data from means of Garden A and B
plot(ozone, ylim=c(0, 8), ylab="y", xlab="order", pch=19, bg=as.numeric(garden))
abline(h=mean(ozone[garden=="A"]))
abline(h=mean(ozone[garden=="B"]), col="red")

# segments
# this code with index[i] works since data was plotted in order
index <- 1:length(ozone)
for(i in 1:length(index)) {
  if(garden[i] == "A")
    lines(c(index[i], index[i]), c(mean(ozone[garden=="A"]), ozone[i]))
  else
    lines(c(index[i], index[i]), c(mean(ozone[garden=="B"]), ozone[i]), col="red")
}


# ANOVA analysis: is this difference in mean ozone in A and B significantly big? Or could it have occurred by chance alone?
# IMPORTANT: if means are significantly different, then variation within sample is smaller than variation between means of all samples. 

# SSE = variation within the sample (made of many SSEs for each sample)
# SSA = variation between each sample mean
# SSY = SSA + SSE
SSY <- sum((ozone - mean(ozone))^2); SSY
SSE <- 
  sum((ozone[garden=="A"] - mean(ozone[garden=="A"]))^2) +
  sum((ozone[garden=="B"] - mean(ozone[garden=="B"]))^2); SSE
SSA <- SSY - SSE; SSA

# build ANOVA table
varY <- SSY/19; varY # total
varE <- SSE/18; varE # error
varA <- SSA/1; varA # garden

"
 H0: mu1 = mu2 = mu3 .... 
 H1: at least one mean from the sample groups is different"
# If the variation between sample means is significantly greater than variation within each sample, then the  means must be different. 
f.ratio <- varA/varE; f.ratio
1 - pf(f.ratio, df1=1, df2=18) # therefore, the two means are significantly different

# Do the easy way
summary(aov(ozone~garden))
summary(lm(ozone~garden))
# df residuals = k(n-1) = (2 garden levels) * (10 replicates per garden - 1) = 2(10-1) = 2* 9 = 18


# Do graphical check of assumptions of model (constant variance and normal errors)
plot(aov(ozone~garden))
# first plot - shows that variances are identical in the two treatments
# second plot: values within each treatment group are normal
# third plot: residuals show constant variance
# fourth plot: attention to the values with large residuals




# Finding SSA directly (shortcut)
cbind(ozone[garden=="A"], ozone[garden=="B"])
tapply(ozone, garden, sum)
T1 <- 30; T2 <- 50

# shortcut SSA = sum(Ti^2)/n - (sum(y))^2/(kn)
SSA <- (30^2 + 50^2)/10 - (sum(ozone))^2/(2*10); SSA

# long way SSA <- n * sum((individual means - overall mean)^2)
SSA <- 10 * sum((mean(ozone[garden=="A"]) - mean(ozone))^2 + 
           (mean(ozone[garden=="B"]) - mean(ozone))^2); SSA

# SSY = sum((y - overall mean)^2)
SSY <- sum((ozone - mean(ozone))^2); SSY

# SSE = sum((y - individual mean)^2) for each of the k levels in the factor
SSE




# Effect sizes of different factor levels
summary(lm(ozone~garden))
summary.lm(aov(ozone~garden)) # these two lines are same things

# pg 160
