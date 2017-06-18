## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions thatcache the inverse of a Matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            Inv <- NULL
            set <- function(y) {
                     x <<- y
                   Inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) Inv <<- solve
  getinverse <- function() Inv
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
  
}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

CacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       Inv <- x$getinverse()
        if(!is.null(Inv)) {
                 message("getting cached data")
                 return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinverse(Inv)
  Inv
}
       

