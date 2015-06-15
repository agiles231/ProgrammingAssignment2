## Put comments here that give an overall description of what your
## functions do

## This is a function that creates a list that has properties
## of a matrix. The resulting list has functions for setting
## and getting the inverse of the matrix, and setting and
## getting the values of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setInverse = setInverse, set = set, get = get
       , getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    print("Inverse was already cached...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
