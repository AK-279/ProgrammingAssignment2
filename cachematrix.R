## Put comments here that give an overall description of what your
## functions do

## See below for the 2 functions for this assignment based on sample makeVector and cachemean examples given, with just minor changes to go from mean to inverse
## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    ## setting the value of the vector, NULL to start with
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## getting the value of the vector
  setInverse <- function(Inverse) m <<- Inverse  ## steps to set and get the value of the Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {  ## first cache date
  m <- x$getInverse()
  if(!is.null(m)) {   ## checking if inverse is NULL or otherwise
    message("getting cached data")
    return(m)   ## step to return inverse value
  }
  data <- x$get()
  m <- solve(data, ...)  ## calculate inverse value
  x$setInverse(m)
  m
}
