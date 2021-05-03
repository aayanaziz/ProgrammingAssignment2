## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Writes functions for getting and hardcoding the
## input matrix and inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## function checks if data is present in cache
## if not then it calculates and returns the 
## inverse

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
}