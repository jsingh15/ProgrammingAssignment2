##function makeCacheMatrix stores a matrix and its inverse
## also stores function to get original matrix, get inverse, populate inverse
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  getinverse <- function() xinv
  ## rather than using setinverse, use populateinverse
  ## populateinverse ensures that it is not possible to set the value of inverse directly
  populateinverse <- function() xinv <<- solve(x)
  list(set = set, get = get,
       getinverse = getinverse,populateinverse = populateinverse)
}
 
cacheSolve <- function(x, ...) {
  xinv <- x$getinverse()
  ## check if inverse is already stored
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  ## compute the inverse and store it
  x$populateinverse()
  message("computed inverse")
  ## get the inverse (we know it is there)
  xinv <- x$getinverse()
  xinv
}
 