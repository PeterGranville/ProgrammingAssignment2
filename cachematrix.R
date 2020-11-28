## makeCacheMatrix and cacheSolve can save time when
## inverting the same large matrix multiple times.  

## makeCacheMatrix creates a list that caches the matrix, gets the 
## value of the matrix, sets the inverse of the matrix, and gets
## the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setinverse <- function(solve){m <<- solve }
  getinverse <- function(){m}
  list(set = set, 
       get = get,               
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will call the cached value of the inverted matrix, 
## if it exists. If the cached inverted matrix doesn't exist, it will
## invert the matrix itself. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mtrx <- x$get()
  m <- solve(mtrx, ...)
  x$setinverse(m)
  m
}
