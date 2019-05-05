## Functions for a special matrix object that can cache it's own inverse
## make and solve functions are for creating the object and then solving
## the inverse of the matrix.


## Instantiate list to hold object, methods, and to cache matrix's inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Instantiate a list to hold matrix x and it's inverse (once computed),
  ## aswell as get and set methods.
  
  # Init holder variables
  inv <- NULL

  # Define methods
  set <- function(y) {
    # Assign values to variables in parent environment
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Return list of named variables and methods to parent (global) environment
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Retrieve or set the inverse from the object

cacheSolve <- function(x, ...) {
  ## Populate or retrieve inverse matrix of makeCacheMatrix() object

  # Try to retrieve inverse from  makeCacheMatrix() object x
  inv <- x$getinverse()

  # If inverse retrieved is null
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # Use x's get() method to retrieve matrix
  data <- x$get()
  # Compute inverse of matrix
  inv <- solve(data, ...)
  # Set inverse using x's setinverse() method
  x$setinverse(inv)
  # Return inverse
  inv
}