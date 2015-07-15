setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RTutorial")
getwd()

w1 <- read.csv(file='w1.dat', sep=",", head=TRUE)
names(w1)
tree <- read.csv(file='trees91.csv', sep=",", head=TRUE)
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
"*****NOT UNDERSTOOD YET!!!"
hist(w1$vals, breaks=12, xlim=c(0, 10))
hist(w1$vals, breaks=12, xlim=c(-1, 2))
hist(w1$vals, breaks=12, xlim=c(0, 2))
hist(w1$vals, breaks=12, xlim=c(1, 1.3))
hist(w1$vals, breaks=12, xlim=c(0.9, 1.3))


# adding strip chart
hist(w1$vals, main="Leaf Biomass in High CO2 Environment", 
     xlab="Biomass of Leaves", xlim=c(0, 2), ylim=c(0, 17))
stripchart(w1$vals, add=TRUE, at=15.5)





# Boxplots
boxplot(w1$vals, main="Leaf BioMass in High CO2 Environment", 
        ylab="BioMass of Leaves")

#

hist(w1$vals,main='Leaf BioMass in High CO2 Environment',
     xlab='BioMass of Leaves',
     ylim=c(0,20))
boxplot(w1$vals, main="Leaf BioMass in High CO2 Environment",
        horizontal=TRUE,
        add=TRUE,
        at=18, 
        axes=FALSE)
stripchart(w1$vals, add=TRUE, at=15)

#

tree$C <- factor(tree$C)
tree$N <- factor(tree$N)

boxplot(tree$STBM, 
        main="Stem BioMass in Different CO2 Environments", 
        ylab="BioMass of Stems", 
        horizontal=TRUE)

# how to create different boxplots for each factor level in C
boxplot(tree$STBM~tree$C)





# Scatter Plots

plot(tree$STBM, tree$LFBM, 
     main="Relationship Between Stem and Leaf Biomass",
     xlab="Stem Biomass", 
     ylab="Leaf Biomass")
#returns correlation coefficient (r)
cor(tree$STBM, tree$LFBM)





# Normal QQ Plots

qqnorm(w1$vals, 
       main="Normal Q-Q Plot of the Leaf Biomass", 
       xlab="Theoretical Quantiles of Leaf Biomass", 
       ylab="Sample Quantiles of Leaf Biomass")
qqline(w1$vals)
