## Programming Assignement 2 for the "R programming" Coursera course by Roger D. Peng, Jeff Leek, Brian Caffo
## Example of a matrix inversion procedure using cached values

## Build an object (list of functions) that allow the environement to 
## hold the cached inverse of the argument matrix
## Created object contains functions to get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # variable to hold the inverse matrix
  inv <- NULL 
  # update x to a new value 
  # and reinitialise inv because it is obsolete if x has changed
  set <- function(y) {
    x <<- y 
    inv <<- NULL 
  }
  # return the current value of x
  get <- function() x 
  # memorize the inverse matrix
  setsolve <- function(solve) inv <<- solve 
  # get the current value of the inverse matrix
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculate the inverse of a matrix using makeCacheMatrix
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  # check if inverse has been calculated
  inv <- x$getsolve()
  # inv is not NULL when solve has been called once already on x
  if(!is.null(inv)) {
    message("getting cached data")
    # inv contains the inverse of x
    return(inv)
  }
  # if inv is NULL, we need to apply solve to x
  data <- x$get()
  inv <- solve(data, ...)
  # and update inv for when we want to access this result later
  x$setsolve(inv)
  # inv contains the inverse of x
  inv
}
