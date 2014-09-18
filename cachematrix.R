## The functions below demonstrate the principles of cache usage that can help to enhance the
## performance of the function that finds the inverse for the given matrix.

## makeCacheMatrix creates a vector (a list of functions) containing the functions to set
## the matrix, get the matrix, set the inverse matrix and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(invmat) inv <<- invmat
  getinv <- function() inv
  
  list(set = set, get = get, setinv=setinv, getinv = getinv)
  
  
}

## cacheSolve allows to work with the list created by makeCacheMatrix functions in such a way
## that if the inverse matrix for the set matrix is already calculated (the inv value is not 
## null), the function returns the existing value; in opposite case the function calculates
## the inverse and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
