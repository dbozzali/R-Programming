## This function creates a special "matrix" object that can cache its inverse.
## Returned object is actually a list containing functions to:
## - set: set the value of the matrix
## - get: get the value of the matrix
## - setinverse: set the value of the inverse of the matrix
## - getinverse: get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(invm) invx <<- invm
  getinverse <- function() invx
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  invx <- x$getinverse()
  if (is.null(invx)) {
    data <- x$get()
    invx <- solve(data, ...)
    message("set solution in cache")
    x$setinverse(invx)
  }
  else {
    message("get cached solution")
  }
  ## Return a matrix that is the inverse of 'x'.
  invx
}
