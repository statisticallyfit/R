
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




# Input vector (v) of values
# Finds the t-(tau)statistic from regression deltaValue = laggedValue + error
# k = number of diffed lags needed to reduce autocorrelation in errors
# ct = constant and trenddickeyFullerTest <- function(v, useTrend=FALSE, k=0)
dickeyFullerTest <- function(v, useTrend=FALSE, k=0){
      n <- length(v)
      #v_1 <- c(NA, v[1:(n-1)])
      #dv <- c(NA, diff(v))
      #data <- data.frame(dv=dv, v_1=v_1)
      
      # Creating the diffed lags
      data.diffedLag0 <- makeDiffedLags(v, from=0, to=k)
      data.lag1 <- makeLags(v, from=1, to=1)
      data <- cbind(data.lag1, data.diffedLag0)
      if(useTrend)
            data$trend <- seq(1:n)
      #if(k != 0){
      #      c <- ncol(data) + 1
      #      for(i in 1:k){
      #            diffedLag <- c(rep(NA, i), dv[1:(n-i)])
      #            data[, c] <- diffedLag
      #            c <- c + 1
      #      }
      #}
      # Find the regression 
      numCols <- ncol(data)
      lm <- ""
      if(numCols == 2){
            lm <- lm(dv ~ v_1, data=data)     
      }else {# error - just need to make sure to get right tau and right reg
            lm <- lm(dv ~ ., data=data[ , 2:numCols])
      }      
      tau <- summary(lm)$coefficients[2,3]
      return(tau)
}


# v = dataframe with one column on values
# k from and to = from and to limits (lags 2 to 4, for example)
makeDiffedLags <- function(v, from, to) {
      n <- nrow(v)
      name <- names(v)
      vec <- v[,1]
      dv <- c(NA, diff(vec))
      data <- data.frame(dv=dv)
      #firstLabel <- paste("d", name, sep="")
      #data <- data.frame(firstLabel = dv)
      col <- ncol(data) + 1
      Vcounter <- 2
      for(i in from:to){
            diffedLag <- c(rep(NA, i), dv[1:(n-i)])
            actualLabel <- paste("d", name, "_", i, sep="")
            changeLabel <- paste("V", Vcounter, sep="")
            data[, col] <- diffedLag
            names(data)[names(data) == changeLabel] <- actualLabel
            col <- col + 1
            Vcounter <- Vcounter + 1
      }
      data$dv <- NULL
      return (data)
}

makeLags <- function(v, from, to){
      n <- nrow(v)
      name <- names(v)
      vec <- v[,1]
      data <- data.frame(v=vec)
      c <- ncol(data) + 1
      Vcounter <- 2
      for(i in from:to){
            lag <- c(rep(NA, i), vec[1:(n-i)])
            actualLabel <- paste(name, "_", i, sep="")
            changeLabel <- paste("V", Vcounter, sep="")
            data[, c] <- lag
            names(data)[names(data) == changeLabel] <- actualLabel
            c <- c + 1
            Vcounter <- Vcounter + 1
      }
      data$v <- NULL
      return (data)
}

# v1, v2 = the data vectors that are supposedly cointegrated
cointegrationTest <- function(v1, v2){
      cointegration.lm <- lm(v1 ~ v2)
      # from urca package
      ur.df(cointegration.lm$residuals, type="none", lags=0)@teststat[1]
}
