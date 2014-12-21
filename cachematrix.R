# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                          # Initialise a variable to hold the inverse
  set <- function(y) {                                 # value assignments
    x <<- y                                            # Use out of environment operator
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse      # Ah, the brain haemorrage inducing joys of
  getInverse <- function() inv                         # lexical scoping #bringBackCobol!
  list(set = set, get=get, setInverse = setInverse, getInverse=getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Inverse exists, retrieving cache...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

# Code to perform a test run

set.seed(42)                                   # Make the test reproducible
x <- matrix(rnorm(25), nrow = 5)               # Generate a matrix x (must be square!)
myCache <- makeCacheMatrix(x)                  # Turn it into a cached matrix
myCache$get()                                  # Return the matrix (confirm same as x)
cacheSolve(myCache)                            # Return the inverse (first run, now cached)
cacheSolve(myCache)                            # Second run - confirm return the cache (message) and co
                                               # confirm is smae as previous.

