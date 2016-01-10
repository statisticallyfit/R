# Data sets from: 
# www.bnlearn.com/book~crc/


library(bnlearn)
#install.packages("bnlearn")


# directed acyclic graph (dag)
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "Tr"))
dag

# Age influences education - younger people have university degree
dag <- set.arc(dag, from = "A", to = "E")
dag
# Gender influences education; women outnumber/outperform men
dag <- set.arc(dag, from="S", to="E")
dag
# Education influences Occupation and Residence
dag <- set.arc(dag, from="E", to="O")
dag <- set.arc(dag, from="E", to="R")
dag
# Occupation and Residence both influence transport method
dag <- set.arc(dag, from="O", to="Tr")
dag <- set.arc(dag, from="R", to="Tr")
dag

modelstring(dag)
nodes(dag)
arcs(dag)

# Less-cumbersome method to set arcs to nodes
dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "Tr"))
arc.set <- matrix(c("A", "E", 
                    "S", "E", 
                    "E", "O", 
                    "E", "R", 
                    "O", "Tr", 
                    "R", "Tr"), 
                  byrow=TRUE, ncol=2, 
                  dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set
all.equal(dag, dag2)

# Confirm acyclic (not circular)
try(set.arc(dag, from="Tr", to="E"))



# Introduce probabilities: levels for these variables (node==variable)
A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
Tr.lv <- c("car", "train", "other")

## Making the local distributions

A.prob <- array(c(0.30, 0.50, 0.20), dim=3, 
                dimnames = list(A = A.lv))
A.prob
S.prob <- array(c(0.60, 0.40), dim=2, 
                dimnames=list(S=S.lv))
S.prob

# Two dimensional table for mixture of Occupation and Education
O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim=c(2,2), 
                dimnames = list(O = O.lv, E = E.lv))
O.prob
R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim=c(2,2), 
                dimnames = list(R = R.lv, E = E.lv))
R.prob
# different syntax but same as...
R.prob <- matrix(c(0.25, 0.75, 0.20, 0.80), ncol=2, 
                 dimnames=list(R=R.lv, E=E.lv))


# Education and Travel are 3-dim tables since they have 2 parents each
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64, 
                  0.36, 0.70, 0.30, 0.90, 0.10), dim=c(2, 3, 2), 
                dimnames = list(E = E.lv, A = A.lv, S = S.lv))
E.prob
Tr.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58, 
                   0.24, 0.18, 0.70, 0.21, 0.09), dim=c(3, 2, 2), 
                 dimnames = list(Tr = Tr.lv, O = O.lv, R = R.lv))
Tr.prob


# Making a new dag with same modelstring
dag3 <- model2network(modelstring(dag2))
dag3
all.equal(dag, dag3)

## Combine dag and local distributions into object of class bn.fit
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, 
            R = R.prob, Tr = Tr.prob)
bn <- custom.fit(dag, cpt)
bn
nparams(bn)
arcs(bn) #note: bn fit can be treated as if it were of class bn
# printing the conditional prob table for R
bn$R
bn$E
R.cpt <- coef(bn$R); R.cpt



## Estimating the parameters:
