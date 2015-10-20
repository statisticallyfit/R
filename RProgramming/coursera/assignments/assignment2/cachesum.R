makeSumVector <- function(x = numeric()) {
      s <- NULL
      set <- function(y){
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsum <- function(theSum) s <<- theSum
      getsum <- function() s
      list(set=set, get=get, setsum=setsum, getsum=getsum)
}

cacheSum <- function(x, ...){
      s <- x$getsum()
      if(!is.null(s)){
            message("getting cached sum...")
            return(s)
      }
      data <- x$get()
      s  <- sum(data, ...)
      x$setsum(s)
      message("caching the data")
      
      return(s)
}