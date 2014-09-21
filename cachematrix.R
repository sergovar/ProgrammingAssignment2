## This is the second programming assignment demonstrating the use of R scoping rules for creating of
## state-preserving object in order to avoid repeating costly calculations.
## In this particular case, the object is a "matrix" caching its inverted matrix. It is presupposed that 
## these functions will be used with invertible matrices only.

## The function makeCacheMatrix creates a special "matrix", which is a list containing two pairs of 
## functions to:
## set the contents of the matrix
## get the contents of the matrix
## set the inverted matrix
## get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  invma <- NULL
  set <- function(y) {
    x <<- y
    invma <<- NULL
  }
  get <- function() x
  setInverted <- function(inv) invma <<- inv
  getInverted <- function() invma
  list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}


## cacheSolve computes the inverse of the special "matrix" x.
## If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverted()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverted(inv)
  inv         ## Return a matrix that is the inverse of 'x'
}
