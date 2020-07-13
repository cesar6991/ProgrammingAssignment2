## I write a pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve= getsolve)

}
## The following function calculates the inverse of the matrix created with the above function. 

cacheSolve <- function(x, ...) {
    m <- x$getsolve() # it first checks to see if the inverse has already been calculated
    if(!is.null(m)) {
      message("getting cached data") 
      return(m)
    }
    data <- x$get() #Otherwise, it calculates the inverse of the data and sets the value 
    m <- solve(data, ...)
    x$setsolve(m)
    m 
  }
  
   ## Return a matrix that is the inverse of 'x'
