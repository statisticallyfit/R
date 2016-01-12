library(bnlearn)
library(Rgraphviz)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnbayesian/MarcoScutari_Bayesian Networks with Examples in R")


survey <- read.table("data/survey.txt", header=TRUE)
head(survey)


### Conditional Independence Tests: identify presence of individual arcs
# if H0 for conditional indep is rejected, arc is included in DAG
df <- (nlevels(survey[, "T"]) - 1) * (nlevels(survey[, "E"]) - 1) * 
      nlevels(survey[, "O"]) * nlevels(survey[, "R"]); df

