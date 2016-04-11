
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


# same SSE as for ANOVA as one from SUMMARY
SSE <- function(y, yhat) {
      return(sum( (y - yhat)^2 ))
}
# OR
#a <- anova(lm)
#ss <- a$`Sum Sq`
#return(ss[length(ss)])
# OR
#s <- summary(r.lm)
#(1-s$r.squared)*SST(var$datamat$dc)

SST <- function(y) {
      return(sum( (y - mean(y))^2 ))
}



# Given restricted and unrestricted models, 
# do joint F-test but give Chi square instead of F
# J = number of restrictions
anovaChiSquare <- function(r.lm, u.lm, J){
      F.stat.list <- anova(r.lm, u.lm)$F
      F.stat <- F.stat.list[length(F.stat.list)]
      chi.stat <- F.stat * J
      p.value <- 1-pchisq(chi.stat, df=J)
      cat("##################################################\n")
      cat("######        Analysis Of Variance         #######\n")
      cat("##################################################\n")
      cat("                                                  \n")
      cat(" Chi-statistic:                        ", chi.stat, "\n")
      cat(" F-statistic:                          ", F.stat, "\n")
      cat(" p-value:                              ", p.value, "\n")
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

# data[,1] = y, data[,2]=x holds the data vectors that are 
#     supposedly cointegrated
# returns the lagged residuals to make it useful for estimating VEC model
# CORRECTION: now returns residuals from dickey fuller test of 
#     cointegrating residuals to know how many lags to include
# Precondition: in data, y is first col, x is second, time is third
# type is either "none", "constant", or "trend"
# lags = number of lags to include in dickey fuller test of stationary 
# for the cointeg.lm's residuals
cointegrationTest <- function(data, type, resid.lags = 0){
      yName <- names(data)[1]
      xName <- names(data)[2]
      y <- data[,1]
      x <- data[,2]
      if(type == "trend") t <- data[,3]
      
      cointeg.lm <- ""
      if (type == "none")          cointeg.lm <- lm(y ~ x + 0)
      else if (type == "constant") cointeg.lm <- lm(y ~ x)
      else if (type == "trend")    cointeg.lm <- lm(y ~ x + t)
      
      # from urca package
      ur <- ur.df(cointeg.lm$res, type="none", lags=resid.lags)
      testStatistic <- ur@teststat[1]
      df.resids <- ur@res
      
      cat("##################################################\n")
      cat("######         Cointegration Test          #######\n")
      cat("##################################################\n")
      cat("                                                  \n")
      cat(" Cointegrating equation: \n")
      
      if (type == "none"){
            b <- cointeg.lm$coefficients[[1]]
            cat(" ", yName, " = ", b, " (", xName, ")", "\n", sep="")
      }
      else if(type == "constant"){
            a <- cointeg.lm$coefficients[[1]]
            b <- cointeg.lm$coefficients[[2]]
            cat(" ", yName, " = ", a, " + ", b, " (", xName, ")", 
                "\n", sep="")
      } else if (type == "trend"){
            a <- cointeg.lm$coefficients[[1]]
            b <- cointeg.lm$coefficients[[2]]
            tCoef <- cointeg.lm$coefficients[[3]]
            cat(" ", yName, " = ", a, " + ", b, " (", xName, ")", 
                " + ", tCoef, " (t)","\n", sep="")
      }
      
      cat("                                                  \n")
      
      criticalValue <- 0
      if (type == "none")          criticalValue <- -2.76
      else if (type == "constant") criticalValue <- -3.37
      else if (type == "trend")    criticalValue <- -3.42
      cat(" test statistic:                        ", testStatistic, "\n")
      cat(" critical value:                        ", criticalValue, "\n")
      
      if (testStatistic > criticalValue)
            cat("\n\n Result: Not cointegrated -> spurious regression")
      else 
            cat("\n\n Result: Cointegrated")
      #return (invisible(cointeg.lm$residuals))
      return (invisible(df.resids))
}

# data must only have two columns (y first then x)
# type must either be: "none", "const", "trend", or "both"
VEC <- function(data, type, lag = 0){
      v <- suppressWarnings(VECM(ts(data), lag=lag, 
                            r=1, include=type, estim="2OLS"))
      print(v$coefficients)
      v$residuals <- data.frame(y=v$residuals[,1], x=v$residuals[,1])
      return(invisible(v$residuals))
}

