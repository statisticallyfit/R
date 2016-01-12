library(bnlearn)
library(Rgraphviz)

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

# convergent connection
dsep(dag, x = "A", y = "S")
dsep(dag, x = "A", y = "S", z = "E")
