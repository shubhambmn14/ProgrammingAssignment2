## Matrix inversion is usually a costly computation and there may be some benefit to caching 
##the inverse of a matrix rather than compute it repeatedly. Below programs writes a pair of functions that cache the inverse of a matrix.

## Caching the Inverse of a Matrix
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setv <- function(y)
  x <<- y
  inverse <<- NULL
  
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
    
  list(setv=setv, get=get, setinverse=setinverse, getinverse=getinverse)

}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse))
    message("getting cached data")
  return(inverse)
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
