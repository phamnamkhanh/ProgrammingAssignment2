## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## This file provides a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## the value of inverser matrix is stored in the variable 'inv'
  inv <- NULL
  
  ## this function sets the original value of matrix 'x'
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## this function provides the original value of matrix 'x'
  get <- function() x
  
  ## this function store the value of inverser matrix of 'x' 
  ## to variable 'inv', when it is computed the 1st time
  set_inv <- function(solve) inv <<- solve
  
  ## if the inverse matrix is computed previously, this function 
  ## get the value of inverser matrix of 'x', which is stored in variable 'inv'
  get_inv <- function() inv
  
  ## The output of this function is a special "matrix", which is 
  ## really a list containing abovementioned functions
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)

}


## The following function calculates the inverse of the special 'matrix'
## created with the above function.

cacheSolve <- function(x, ...) {
        
  ## try to get the inverse matrix from the cache first  
  inv <- x$get_inv()
  
  ## if the cache is available, return the value of inverse matrix and inform the message 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if the cache is not available, store the original matrix to variable 'data'
  data <- x$get()
  
  ## compute the inverser matrix using solve() and store to variable 'inv'
  inv <- solve(data)
  
  ## store the inversre matrix to the cache
  x$set_inv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
  
  
}
