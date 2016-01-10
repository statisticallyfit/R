library(ggplot2)


# Example: Texting while driving (binomial)

drive <- c("texting", "texting", "texting", "not", "not", 
           "texting", "texting", "not", "not", "texting")

driveNum <- ifelse(drive == "texting", 1, 0)
N <- length(drive)
nTexting <- sum(drive == "texting")
nNot <- sum(drive == "not")

x1 <- rbinom(1000, size=10, p=0.5)
x2 <- rbinom(1000, size=10, p=0.85)

mean(x1); hist(x1)
mean(x2); hist(x2)


# Goal: estimate theta, probability that says whether driver is texting
theta <- seq(from=1/(N+1), to=N/(N+1), length=10); theta

### PRIOR: triangular distribution to put weight at 0.5
## uniform: pTheta = dunif(theta)
## triangular: 
pTheta = pmin(theta, 1-theta); pTheta
## beta with mean=0.5: pTheta = dbeta(theta, 10, 10)

### LIKELIHOOD: P(y | theta)
pDataGivenTheta <- choose(N, nTexting) * theta**nTexting * (1-theta)**nNot
pDataGivenTheta

### POSTERIOR:
pData <- sum(pDataGivenTheta * pTheta); pData
pThetaGivenData <- pDataGivenTheta * pTheta / pData
pThetaGivenData; hist(pThetaGivenData)


round(data.frame(theta, prior=pTheta, likelihood=pDataGivenTheta, 
                 posterior=pThetaGivenData), 3)

posteriorMean <- sum(pThetaGivenData * theta)
posteriorMean
