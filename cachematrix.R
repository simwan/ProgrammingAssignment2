## Caching the inverse of a matrix:
## as matrix inversion usually is a costly computation,
## caching of the inverse matrix might be a lot simpler 
## and faster than repeated computing. 

## This function creates a "special" matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y){
    x <<- y
    Inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) Inv <<- inverse
  getInverse <- function() Inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the "special" matrix created by the previous function (makeCacheMatrix)
## If the inverse has been calculated, the cacheSolve will 
## retrieve the inverse from cache

cacheSolve <- function(x, ...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)){
    message("Cached data")
    return(Inv)
  }
  mat <- x$get()
  Inv <- inverse(mat, ...)
  x$setInverse(Inv)
  Inv
}
