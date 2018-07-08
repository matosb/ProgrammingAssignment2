## Two functions in this R script
## makeCacheMatrix creates a special "matrix", which is list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the reverse matrix
##  get the value of the reverse matrix

## cacheSolve calculates the reverse matrix of the special "matrix" created 
## with the above function. However, it first checks to see if the reverse matrix
## has already been calculated. If so, it gets the reversematrix from the cache 
## and skips the computation. Otherwise, it calculates the reverse matrix and sets 
## the value in the cache via the setinv function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
