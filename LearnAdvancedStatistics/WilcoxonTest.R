# WILCOXON RANK SUM TEST (small samples)

" H0: distributions for populations 1 and 2 are identical
  Ha: distributions for pop1 and 2 are different (two tailed)
      or distribution for pop1 lies to left of pop2's (left tailed)
      or distribution for pop1 lies to right of pop2's (right tailed)"

"PROCEDURE: 
1. Rank all n 1 ϩ n 2 observations from small to large.

2. Left tailed test statistic: TL = rank sum of observations in n1.

3. Right tailed test statistic: TR = n1(n1 + n2 + 1) - TL
This is the rank sum of group n1 if the ranks had been reversed from big to small. 

4. Two tailed test statistic: T = min of TR and TL.

5. H0 is rejected if the observed test statistic is beyond the critical
value found using Table 7 in Appendix I.
"

# Example 1

data <- c(169, 178, 180, 180, 182, 185, 188, 190, 225, 235)
species <- c(2,2,2,2,2,2,1,1,1,1)
rank <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
wingstroke <- data.frame(Data=data, Species=species, Rank=rank)
wingstroke

" H0: distributions of wing stroke frequencies are the same for the two species
  Ha: distributions of wing stroke frequencies differ for the two species"

TL <- sum(wingstroke$Rank[wingstroke$Rank >=7])

n1 <- length(wingstroke$Species[wingstroke$Species == 1])
n2 <- length(wingstroke$Species[wingstroke$Species == 2])
TR <- n1*(n1 + n2 + 1) - TL 

Tcrit <- 12 #assuming alpha=0.05, look at Table 7 in mendenhall
Tstatistic <- min(TR, TL)

if(Tstatistic <= Tcrit)
  "Reject null"



# WILCOXON RANK SUM TEST (uses normal approximation)(large samples, n1>= 10, n2>= 10)


" Test statistic: z = (T - n1(n1 + n2 + 1)/2)/sqrt(n1n2(n1 + n2 + 1)/12)"

"EXAMPLE: 

An experiment was conducted to compare the strengths of two types of kraft papers:
one a standard kraft paper of a specified weight and the other the same standard kraft
paper treated with a chemical substance. Ten pieces of each type of paper, randomly
selected from production, produced the strength measurements shown in the table

H0: identical distributions of strengths
Ha: distribution of strength of standard paper is to left of that of treated paper"

standard <- c(1.21, 1.43, 1.35, 1.51, 1.39, 1.17, 1.48, 1.42, 1.29, 1.40)
treated <- c(1.49, 1.37, 1.67, 1.50, 1.31, 1.29, 1.52, 1.37, 1.44, 1.53)
paper <- data.frame(Standard1=standard, Treated2=treated)

"papersorted <- sort(c(standard, treated))
papersorted
which(papersorted==1.29)"
TL = 85.5
TR = 124.5
n1 = n2 = 10
T = min(TL, TR)
#find mu and stdev
muT <- function(n1, n2){return (n1*(n1 + n2 + 1)/2) }
sigmaT <- function(n1, n2){return (sqrt(n1*n2*(n1 + n2 + 1)/12))}
z <- ((T-muT(n1, n2))/sigmaT(n1, n2))
z
# pvalue P(Z <= z)
p.value <- pnorm(z)
p.value
alpha = 0.05
if(p.value < alpha)
  "Reject null"
"Fail to reject null"

  


