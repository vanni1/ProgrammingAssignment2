## Assignment: Caching the Inverse of a Matrix

##Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(y) {
      x <<- y
      INV <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) INV <<- inverse
    getInverse <- function() INV
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
    

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  INV <- x$getInverse()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- inverse(data, ...)
  x$setInverse(INV)
  INV
}
