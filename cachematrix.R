#############################################################################
## File Name: cachematrix.R
##
## File Description:
##     Matrix inversion is a costly computation so this file contains a pair
##     of functions that cache the inverse of a matrix, the first time it is
##     calculated, so that subsequent calls to calculate the inverse simply
##     return the cached value.
#############################################################################


#############################################################################
## Function Name: makeCacheMatrix
##
## Description: 
## Creates a special "matrix" object that can cache its inverse. 
##
## Arguments:
##    x: a square invertible matrix whose inverse is to be stored in the 
##       returned objects cache. 
##       x defaults to an empty 1x1 matrix, if not provided.
##
## Returns: the special "matrix" object that caches the inverse of "x". 
#############################################################################
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#############################################################################
## Function Name: cacheSolve
##
## Description: 
## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve will retrieve the inverse from the cache.
##
## Arguments:
## x: a special "matrix" returned by makeCacheMatrix that is capable of 
##    caching a square invertible matrix and its inverse.
## ...: any additional arguments will be passed along to the solve function 
##      when the matrix inverse is computed.
##
## Returns: the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve will retrieve the inverse from the cache.
#############################################################################
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

#############################################################################
##                          END OF FILE
#############################################################################

