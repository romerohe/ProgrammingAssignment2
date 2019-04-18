## Those are functions to complete the Programming Assignment number 2.
## Thank you for your time to revise it ;)


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      setmatrix <- function(y) {
            x <<- y
            inv <<- NULL
      }
      getmatrix <- function() x
      setinv <- function(minv) inv <<- minv
      getinv <- function() inv
      list(setmatrix = setmatrix, getmatrix = getmatrix, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$getmatrix()
      inv <- solve(data, ...)
      x$setinv(inv)
      return(inv)
}