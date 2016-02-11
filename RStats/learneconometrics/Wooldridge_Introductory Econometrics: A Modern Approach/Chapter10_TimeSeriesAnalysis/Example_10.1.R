setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/Wooldridge_Introductory Econometrics: A Modern Approach/Chapter10_TimeSeriesAnalysis")
rm(list=ls())


library('foreign')
library(ggfortify)

download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/phillips.dta','phillips.dta',mode="wb")
download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/intdef.dta','intdef.dta',mode="wb")
download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/prminwge.dta','prminwge.dta',mode="wb")
download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta','fertil3.dta',mode="wb")
download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/barium.dta','barium.dta',mode="wb")
download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/fair.dta','fair.dta',mode="wb")
download.file('http://fmwww.bc.edu/ec-p/data/wooldridge/hseinv.dta','hseinv.dta',mode="wb")

# Example 10.1
phillips <- read.dta('phillips.dta')
lm.10.1 <- lm(inf ~ unem, data=phillips)
summary(lm.10.1)

phillips.ts <- ts(phillips[,2], start=1948) # how to set names?
autoplot(lm.10.1)
autoplot(phillips.ts, ts.size = 1, ts.colour = "red")
#ggplot(phillips.ts, aes(year, unem)) + geom_line(lwd=1) + xlab("year") + ylab("unem")


# Example 10.2
intdef <- read.dta("intdef.dta")
lm.10.2 <- lm(i3 ~ inf + def, data=intdef)
summary(lm.10.2)

lm.10.2$residuals
# OR
intdef$i3 - lm.10.2$fitted.values
lm.10.2$fitted.values
lm.10.2$coefficients
lm.10.2$model

intdef.ts <- ts(data.frame(intdef[,2:3], def=intdef[,6]), start=1948)
head(intdef.ts)
autoplot(intdef.ts, ts.size=1, ts.colour="blue")
# Or individual ggplots
#ggplot(intdef.ts, aes(year, i3)) + geom_line(lwd=1) + xlab("year") + ylab("i3")
#ggplot(intdef.ts, aes(year, inf)) + geom_line(lwd=1) + xlab("year") + ylab("inf")
#ggplot(intdef.ts, aes(year, def)) + geom_line(lwd=1) + xlab("year") + ylab("def")



# Example 10.3
prminwge <- read.dta('prminwge.dta')

lm.10.3 <- lm(lprepop ~ lmincov + lusgnp, data=prminwge)
summary(lm.10.3)

prminwge.ts <- ts(data.frame(lprepop=prminwge$lprepop, lmincov=prminwge$lmincov, 
                             lusgnp=prminwge$lusgnp), start=1950)
prminwge.ts
autoplot(prminwge.ts, ts.size=1)




# Example 10.4
fertil3 <- read.dta('fertil3.dta')
# view the data
fertil3.ts <- ts(data.frame(gfr=fertil3$gfr, pe=fertil3$pe, 
                            pe_1=fertil3$pe_1, pe_2=fertil3$pe_2, 
                            pill=fertil3$pill, ww2=fertil3$ww2), start=1913)
autoplot(fertil3.ts, ts.size=1, ts.colour="deeppink1")

# create the models
lm.10.4.1 <- lm(gfr ~ pe + ww2 + pill, data=fertil3)
summary(lm.10.4.1)

# multicollinearity between pe, pe1, pe2 makes it hard to estimate their coefs
lm.10.4.2 <- lm(gfr ~ pe + pe_1 + pe_2 + ww2 + pill, data=fertil3)
lm.10.4.2
summary(lm.10.4.2)

# Joint significance of pe values
lm.10.4.2res <- lm(gfr ~ ww2 + pill, data=fertil3, subset=(is.na(pe_2)==FALSE))
lm.10.4.2res #called the restricted model, and lm.10.4.2 is the unrestricted model
lm.10.4.2
anova(lm.10.4.2res, lm.10.4.2)


# Joint significance of lagged pe values
lm.10.4.2res2 <- lm(gfr ~ pe + ww2 + pill, data=fertil3, subset=(is.na(pe_2)==FALSE))
lm.10.4.2res2
lm.10.4.2
anova(lm.10.4.2res2, lm.10.4.2)

# Standard error of the long run propensity (running the theta=LRP regression
# to get standard error of the coefficient on pe)
with(fertil3,
     {
      p1p0 <- pe_1-pe
      p2p1 <- pe_2-pe_1
      print(summary(lm(gfr ~ pe + p1p0 + p2p1 + ww2 + pill)))})
# so theta (LRP) coefficient (on pe) is significant. There is a 
# significant long-run effect of pe. 

