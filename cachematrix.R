## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates
# a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_cache <- NULL
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  get <- function () x
  setInverse <- function(inverse) inv_cache <<- inverse
  getInverse <- function() inv_cache
  list( set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function

#  Function computes the inverse 
#  of the special "matrix" created by function makeCacheMatrix.
#  If the matrix has not change and the inverse 
#  has already been calculated, 
#  function retrive the inverse from chache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_cache <- x$getInverse()
  if (!is.null(inv_cache)) {
      message("Getting inverse from chache")
      retun(inv_cache)
  }
  # create matrix since it does not exist
  matrix <- x$get()
  inv_cache <- solve(matrix, ...)
  x$setInverse(inv_cache)
  return(inv_cache)
}
