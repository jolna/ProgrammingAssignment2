## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Given a square invertible matrix X, the makeCacheMatrix 
## returns the list of functions using which the facilitates the
## caching the inv matrix unsing the functions environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setMatrix <- function (newMData) {
    x <<- newMData
    inv <<- NULL
  }
  
  getMatrix <- function () x
  
  setInverse <- function (newInv) inv <<- newInv
  
  getInverse <- function () inv
  
  list(setM = setMatrix, getM = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse matrix of a given square matrix.
## Function checks to see if the matrix has changed to decide if the 
## inverse needs to be computed or returned from cache 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse matrix")
    return (inv)        
  }
  data <- x$getM()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}