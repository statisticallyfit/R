library(bnlearn)
library(Rgraphviz)
library(gRain)

#install.packages("gRain") #didn't work
biocLite("gRain")

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnbayesian/MarcoScutari_Bayesian Networks with Examples in R")


survey <- read.table("data/survey.txt", header=TRUE)
head(survey)


#Are two ndoes directly separated? (d-separated)
dsep(dag, x = "S", y = "R")
dsep(dag, x = "O", y = "R")
path(dag, from = "S", to="R")

# now conditioning on E, S->E->R is blocked so S -> R directly separated is TRUE
# serial connection
dsep(dag, x = "S", y = "R", z = "E")

# divergent connection
dsep(dag, x = "O", y = "R")
dsep(dag, x = "O", y = "R", z = "E")

# convergent connection: only dsep when not conditioning on E, pointed to by A and S
dsep(dag, x = "A", y = "S")
dsep(dag, x = "A", y = "S", z = "E")



#-------------------------------------------------------------------------------------

### Using conditional prob queries and most likely explanation queries by
### exact and approximate inference

# Exact Inference, EXAMPLE1 - different preference in transport of women vs all?
junction <- compile(as.grain(bn)) #transform bn into junction tree to do inference
junction
# This is P(T)
jsex <- setEvidence(junction, nodes = "S", states = "F"); jsex
querygrain(junction, nodes = "T")$T ###### P(T)
querygrain(jsex, nodes = "T")$T     ###### P(T | S = female)
# conclude: no major differences in probabilities ==> so women show same preferences
# towards car and train use as interviewees as a whole


# Exact Inference, EXAMPLE2 - how living in small city affects car and train use
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(junction, nodes = "T")$T ###### P(T)
querygrain(jres, nodes = "T")$T     ###### P(T | R=small)
#conclude: so people in small residence prefer train more  and car less
# and 'other' more than all people combined


# Exact Inferece, EXAMPLE3 - relation between S and T conditioning on E=high
jedu <- setEvidence(junction, nodes = "E", states = "high"); jedu
SandT.cpt <- querygrain(jedu, nodes = c("S", "T"), type = "joint")
SandT.cpt # This is P(S and T | E = high)

#illustration of types of distributions
querygrain(jedu, nodes = c("S", "T"), type = "marginal") # P(S|E=high) with P(T|E=high)
querygrain(jedu, nodes = c("S", "T"), type = "conditional") # P(S|T|E=high)

# last one: NOTE:
# P(S=F | T=t | E=high) = P(S=F | E=high)
# P(S=M | T=t | E=high) = P(S=M | E=high), which means
# S is independent of T conditional on E. Gender does not tell of transport
# preferences if we know person's education

# method 1 to confirm the above
dsep(bn, x = "S", y = "T", z = "E") 

# method 2 to confirm the above
SandT.ct = SandT.cpt * nrow(survey) # make contingency table
SandT.ct
chisq.test(SandT.ct) # ha absolutely independent! pvalue=1



### Approximate Inference (monte carlo)
