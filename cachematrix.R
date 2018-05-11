## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Assignment: Caching the Inverse of a Matrix 
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Matrix inversion is usually a costly computation and there may be some benefit   
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here)
## Your assignment is to write a pair of functions that cache the inverse of a 
## matrix.
 
## Write the following functions:
   
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, then 
## solve(X) returns its inverse.
 
## For this assignment, assume that the matrix supplied is always invertible.

### This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# set the value of the matrix
  stored <- NULL 
 set <- function(newvalue) {
  x <<- newvalue
  stored <<- NULL
 }
# get the value of the matrix
 get <- function() x
# set the value of the inverse
 setinverse <- function(solve) stored <<- solve
# get the value of the inverse
 getinverse <- function() stored
# return a list
 list(set = set, 
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

### cacheSolve: This function computes the inverse of the special "matrix" 
### returned by makeCacheMatrix above. If the inverse has already been 
### calculated (and the matrix has not changed), then the cachesolve should 
### retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
    # get the cached value
  inv <- x$getinverse()
  # should it exist return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise calculate the inverse of the matrix and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

