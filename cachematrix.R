## This function creates a special matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_mat <<- inv
  getinv <- function() inv_mat
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks if the inverse of the special matrix created
## by the function makeCacheMatrix above has already been calculated. 
## In this case it retrieves the inverse from the cache, otherwise
## it computes the inverse and sets its value in the cache. 

cacheSolve <- function(x) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  xdata <- x$get()
  inv <- solve(xdata)
  x$setinv(inv)
  inv
}
