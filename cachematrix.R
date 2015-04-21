## Here we write a pair of functions that cache the inverse of a matrix

## the following function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }   # set the value of the matrix
  get <- function() x    # get the value of the matrix
  setInverse <- function(inverse) inv <<- inverse  # set the value of the inverse matrix
  getInverse <- function() inv  # get the value of the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }  #  it first checks to see if the inverse has already been calculated
  data <- x$get()
  inv <- solve(data, ...)  # if the inverse is not calculated before, it calculates the inverse of the data 
  x$setInverse(inv)        # and sets the value of the inverse in the cache via the setInverse function
  inv
}
