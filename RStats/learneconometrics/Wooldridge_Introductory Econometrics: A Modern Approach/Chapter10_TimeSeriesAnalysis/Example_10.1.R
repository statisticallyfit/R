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

phillips.ts <- ts(phillips[0:2])
autoplot(phillips.ts, ts.size = 1, ts.colour = "red")
ggplot(phillips.ts, aes(year, unem)) + geom_line(lwd=1) + xlab("year") + ylab("unem")


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

intdef.ts <- ts(data.frame(intdef[,1:3], def=intdef[,6]))
head(intdef.ts)
autoplot(intdef.ts, ts.size=1, ts.colour="blue")
# Or individual ggplots
ggplot(intdef.ts, aes(year, i3)) + geom_line(lwd=1) + xlab("year") + ylab("i3")
ggplot(intdef.ts, aes(year, inf)) + geom_line(lwd=1) + xlab("year") + ylab("inf")
ggplot(intdef.ts, aes(year, def)) + geom_line(lwd=1) + xlab("year") + ylab("def")



# Example 10.3
prminwge <- read.dta('prminwge.dta')

lm.10.3 <- lm(lprepop ~ lmincov + lusgnp, data=prminwge)
summary(lm.10.3)

prminwge.ts <- ts(data.frame(year=prminwge$year, lprepop=prminwge$lprepop, 
                             lmincov=prminwge$lmincov, lusgnp=prminwge$lusgnp))
head(prminwge.ts)
autoplot(prminwge.ts, ts.size=1)
