## Put comments here that give an overall description of what your
## functions do
# The functions create a matrix whose inverse is stored in a variable
# that can be recalled later without having to recompute the value; the
# variable is stored in a variable inside the function in which the matrix
# object was created. The second function is the function that either computes
# the inverse if it has not been computed or retrieves the all ready-computed
# inverse

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


## This function returns the inverse of the matrix by either computing and
## storing the value of the inverse in the inverse variable that is associated
## with the matrix, or retrieving the already computed inverse variable
## associated with the matrix

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
