## This R file contains a pair of functions that cache the inverse of a matrix.
## Background:
## Matrix inversion is usually a costly computation;
## There may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }    
    get <- function() x    
    setInverse <- function(inverse) inv <<- inverse    
    getInverse <- function() inv
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Assumes that the matrix supplied is always invertible.
## ... further arguments are passed to the 'solve' function

cacheSolve <- function(cx, ...) {
    inv <- cx$getInverse()
    if(!is.null(inv)) {
        message("getting cached data..")
        return(inv)
    }
    
    #otherwise need to compute the inverse and cache it
    data <- cx$get()
    inv <- solve(data, ...)
    cx$setInverse(inv)
    inv
}
