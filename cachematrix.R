##This implementation of functions is done in the very way "Example: Caching the Mean of a Vector" was implemented
## All function names are provided in accordance with the guideline provided for function naming

## User needs to call the makeCacheMatrix() function in order to assign the matrix to the special "vector"
## User MUST submit square matrix. The same should not contain consecutive integers at consecutive positions
## of the matrix


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## implementation of the get() function does the job of matrix inversion

## this inversion implementation has its limitation of accepting ONLY square matrix
## althopugh other library with no such limitation could be used, "solve" has been used
## for the sake of simplicity and objective of this assignment

## for the firt time execution no message will be printed
## for repeated call after the first call only show the data from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## implementation of the inversion: start
  m <- solve(data, ...)
  ## implementation of the inversion: end
  x$setinverse(m)
  m
}

