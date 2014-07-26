## This module contains function which enable us to create a matrix capable of
## storing it's inverse once computed, to be reused for next time, and resolver 
## function which is able to work with such construct.
## This can bring positive performance benefits. 

## This function creates a "enriched matrix" which contains utility functions
## to get and set the matrix, and to get and set the inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function is a cachedMatrix resolver.
## It checks if cachedMatrix passed as a input contains
## computed inverse, otherwise it computes it, stores into cachedMatrix
## and returns the inverse itself.
cacheSolve <- function(x, ...) {
  ## do we have a valid input?
  if (!("getinverse" %in% names(x))) {
    stop("input is not valid cachedMatrix")
  }
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## retrieve, compute inverse and set
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
