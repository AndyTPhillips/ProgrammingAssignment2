# makeCacheMatrix creates a list containing a function to
# 1. Set the value of a matrix
# 2. Return the value of a matrix
# 3. Set the value the of the inverse of a matrix
# 4. Return the value of the inverse of the matrix

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


# cachesolve returns the inverse of the matrix, if a cache of the inverse does not already exist.
# If the inverse already exists the function will skip the computation and return the pre-existing
# variable. If it does have to calculate the inverse it caches this value so future calls to the 
# function can bypass the computation.

# The supplied matrix must be invertible.
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

