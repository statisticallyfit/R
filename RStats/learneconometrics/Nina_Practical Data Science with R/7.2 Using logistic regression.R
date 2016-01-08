setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/Nina_Practical Data Science with R")

load("data/NatalRiskData.rData")
head(sdata)

train <- sdata[sdata$ORIGRANDGROUP <= 5, ]
test <- sdata[sdata$ORIGRANDGROUP > 5, ]


complications <- c("ULD_MECO", "ULD_PRECIP", "ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")

y <- "atRisk"
x <- c("PWGT", "UPREVIS", "CIG_REC", "GESTREC3", "DPLURAL", 
       complications, riskfactors)
fmla <- paste(y, paste(x, collapse=" + "), sep=" ~ ")
print(fmla)

model <- glm(fmla, data=train, family=binomial(link="logit"))

# Need to specify "type="response"" otherwise predict() returns
# result of logit(y), need to put this response into sigmoid function
train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")



