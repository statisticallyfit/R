library(ggplot2)

w1 <- read.csv(file='w1.dat', sep=",", head=TRUE)
names(w1)
tree
names(tree)


# Strip Charts
stripchart(w1$vals)
stripchart(w1$vals, method="stack")
stripchart(w1$vals, method="jitter")
stripchart(w1$vals, vertical=TRUE)
stripchart(w1$vals, vertical=TRUE, method="jitter")

stripchart(w1$vals, method="stack",
           main="Leaf Biomass in High CO2 Environment",
           xlab="Biomass of Leaves", 
           ylab="CO2 Concentrations")
title("Leaf Biomass in High CO2 Env") #writes overtop of current




# Histograms
hist(w1$vals)
hist(w1$vals, main="Distribution of w1", xlab="w1")

hist(w1$vals, breaks=4)
hist(w1$vals, breaks=12)

#varying domain
hist(w1$vals, breaks=12, xlim=c(0, 10))
