
## need help, skip for now and develop my own function
# but how would i estimate that first mean equation? 
# data.frame = contains vector of data time series on its single column
TARCH1 <- function(data, p){
      arch <- ARCH(data, p=p)
      # r-hat = mu + errors
      # errors = y-obs - mu, mu = fit function rt
      mu <- rep(arch@fit$coef[[1]], length(y))
      vt <- y - mu # this is e-hat to use in ht
      n <- length(vt)
      h.1 <- 1 #mean(y - mean(y))    # starting value of h
      e.1 <- vt[1] * sqrt(h.1)       # starting value of e
      et <- rep(0, n); et[1] <- e.1 
      ht <- rep(0, n); ht[1] <- h.1
      delta <- sapply(vt, function(elem) {if(elem < 0) 1 else 0})
      
      omega <- 1e-6
      alpha <- 0.1 # specifies model of order 1
      #beta <- 0.8 # specifies model of order 1
      for(i in 2:n) {
            ht[i] <- et[i-1]^2  + delta[i-1] * et[i-1]^2 
            et[i] <- vt[i] * sqrt(ht[i])
      }
      
      lm <- lm(ht^2 ~ (1 + delta) * et^2)
      lm
      
      ## MAJOR HELP
}

