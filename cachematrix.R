## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinverse <- function(imat) mat <<- imat
  getinverse <- function() mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then it retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
