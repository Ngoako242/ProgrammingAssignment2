makeCacheMatrix <-function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
   get <- function() {x}
   setinverse <- function(inverse) {inv <<- inverse}
   getinverse <- function() {inv}
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cachesolve <- function(x,...){
  inv <- xigetInverse()
  if(!is.null(inv)){
    massage("getting cached data")
    return(inv)
  }
  mat <- xiget()
  inv <- solve(mat, ...) 
  x$setinverse(inv)
  inv
}
