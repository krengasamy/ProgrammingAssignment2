## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly (there are also alternatives to matrix inversion that we will not 
## discuss here). Your assignment is to write a pair of functions that cache the 
## inverse of a matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
  {
    x
  }
  ##set the Inverse
  setInv <-function(Inv){
    i <<- Inv
  }
  
  ##Get the Inverse
  getInv <-function(){
    m
  }
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
