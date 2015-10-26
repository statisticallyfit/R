# Profiling code: to examine how much time is for each part of the program
# must know where program is spending most time

# Principles: 
      # design first, then optimize
      # measure (collect data) 

# Tools to measure: 
# 1) system.time() - computes time in seconds needed to evaluate expression
# returns: usertime (charghed by CPU) AND elapsed time (actual time)
# elapsed time > usertime if CPU waits around
# elapsed time < usertime if lots of cores


## Elapsed time > user time
system.time(readLines("http://www.jhsph.edu"))

## Elapsed time < user time
hilbert <- function(n) {
      i <- 1:n
      1/outer(i-1, i, "+")
}
x <- hilbert(1000)
system.time(svd(x))

## Elapsed time ~ user time
system.time({
      n <- 1000
      r <- numeric(n)
      for(i in 1:n){
            x <- rnorm(n)
            r[i] <- mean(x)
      }
})


# --------------------------------------------------------------
# Rprog() and summaryRprof() (DO not use bothsystem.time() and Rprof())

# Rprof() measures time in each function in the call stack
# default sampling interval is 0.02
# summaryRprof() summarizes output of Rprof() and gives % time spent
# using by.total and by.self

## lm(y ~x)
sample.interval = 1000
# two methods for normalizing data: 
      # by.total = divides time spent in each function by total run time
      # by.self = same but first subtracts out time spent in functions
      # above in the call stack (subtracting out time spent in lower
      # level functions)
#$by.total
#$by.self
#$sample.interval
#$sampling.time # same as elapsed time for system.time function