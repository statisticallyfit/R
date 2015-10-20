# the <<- asigns value to object in environment other than current

" This creates list containing functions to: 
      * set value of vector
      * get value of vector
      * set value of mean
      * get value of mean 
"
makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m

      list(set = set, get = get, setmean = setmean, getmean = getmean)
}


" Calculates the mean of the special vector made above
If mean was found, it gets mean from cache. Else, calculates and
sets it in cache via setmean function
"
cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      
      return(m)
}