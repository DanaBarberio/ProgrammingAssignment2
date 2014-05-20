## This program caches the inverse of a matrix by creating a "matrix" object that 
## can cache its inverse, and then computes the inverse of this special "matrix".


## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
      x <<- y
      m <<- NULL
  }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
      setsolve = setsolve,
      getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix function. If the inverse has already been calculated, 
## then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
