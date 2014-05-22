## Put comments here that give an overall description of what your
## functions do

## Creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## To retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  if ( det(data) == 1) {
   i  <- solve(data, ...)
    x$setinverse(i) }
  i
}
