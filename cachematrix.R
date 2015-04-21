## Here we write a pair of functions that cache the inverse of a matrix

## the following function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }   # set the value of the matrix
  get <- function() x    # get the value of the matrix
  setInverse <- function(inverse) m <<- inverse  # set the value of the inverse matrix
  getInverse <- function() m  # get the value of the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }  #  it first checks to see if the inverse has already been calculated
  data <- x$get()
  m <- solve(data, ...)  # if the inverse is not calculated before, it calculates the inverse of the data 
  x$setInverse(m)        # and sets the value of the inverse in the cache via the setInverse function
  m
}
