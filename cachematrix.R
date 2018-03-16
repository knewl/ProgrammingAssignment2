## The functions below can be used to cache the inverse of an invertible matrix. 
## Caching saves the computational time required in re-calculating the inverse multiple times.
## Example use: m <- matrix(c(1,-3,-1,4),nrow=2,ncol=2,byrow=TRUE)
##              cached_m <- makeCacheMatrix(m)
##              inverse_m <- cacheSolve(cached_m) ##calculates inverse, caches it and returns it
##              inverse_m_again <- cacheSolve(cached_m) ##returns cached inverse

## makeCacheMatrix takes an invertible matrix argument 'x' and creates a list of 4 functions : set matrix, get matrix, set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix_inverse) inv <<- matrix_inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calculates the inverse of matrix 'x' an invertible matrix, caches the result and returns it. 
## If the result has already been cached the inverse is retrieved.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
