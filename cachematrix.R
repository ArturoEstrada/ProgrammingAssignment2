
## This function creates a special "vector", which is really a list containing a function to
##        1. set the matrix
##        2. get the matrix
##        3. set the inverse
##        4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
} 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
