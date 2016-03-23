
totalMultiplier <- function(lmObject){
      result <- sum(lmObject$coefficients) - lmObject$coefficients[1]
      return(result[[1]])
}

# K = number of parameters including intercept
modelCriteria <- function(y, yhat, K, lag) {
      sse <- sum( (y - yhat)^2 )
      N <- length(y) # Must equal length of yhat
      aic <- log(sse/N) + 2*K/N
      bic <- log(sse/N) + K*log(N)/N
      aicc <- -(abs(aic) - 1 - log(2*pi))
      bicc <- -(abs(bic) - 1 - log(2*pi))
      cat("\nAIC:  ", aic)
      cat("\nAICc: ", aicc)
      cat("\nBIC:  ", bic)
      cat("\nBICc: ", bicc, "\n ")
      criteriaAndLag <- c(aic, bic, lag)
      return(invisible(criteriaAndLag)) # yaya this works!!!
}



# Written by John Fox to calculate HAC standard errors
# source: http://novicemetrics.blogspot.ro/2011/04/video-tutorial-on-robust-standard.html
summaryHAC <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
      
      if (!require(car)) stop("Required car package is missing.")
      
      type <- match.arg(type)
      V <- hccm(model, type=type)
      sumry <- summary(model)
      table <- coef(sumry)
      table[,2] <- sqrt(diag(V))
      table[,3] <- table[,1]/table[,2]
      table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
      
      sumry$coefficients <- table
      p <- nrow(table)
      hyp <- cbind(0, diag(p - 1))
      sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
      
      print(sumry)
      cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
      
      # the below is added by statisticallyfit
      ## returning HAC standard errors
      return(table[,2])
}
