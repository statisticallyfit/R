"
APPLY FUNCTIONS

      apply(x, MARGIN, FUN, ...) - evaluate function over 
            margins of array
      lapply() and sapply() = evaluate function over list or vector
      tapply(x, INDEX, FUN, ...) - splits vector into groups and 
            does function over them
      split() - splits vector into groups. 
      mapply(FUN, ...) - applies function in parallel over arguments
"

# 1. Create matrix with values from normal distribution. Let
# stdev be equal to col number

mat <- matrix(NA, ncol=5, nrow=50)
for(i in 1:ncol(mat)){
      mat[, i] <- rnorm(50, sd=i)
}
head(mat)

# a) Use apply to find stdev of cols of matrix
apply(mat, 2, sd)
# b) Use apply to find max of each col
apply(mat, 2, max)


#----------------------------------------------------------------
# http://bioinformatics.nki.nl/courses/Rstat_12_I/texts/resources/exercises_apply_LP121206.pdf
# 2) Make list of vectors of varying length
veclen <- sample(11:20)
veclen
mylist <- lapply(veclen, sample)
mylist

# 1. Use for loop to find lengths of vectors in the list

foundLens <- list()
for(i in seq_len(length(veclen))){
      foundLens <- c(foundLens, length())
}


# ----------------------------------------------------------------
# ftp://ftp.stat.math.ethz.ch/U/sfs/RKurs/R.ITA.Advanced/Exercises/ueb-strings-apply.pdf
# a)
data <- read.csv("")
