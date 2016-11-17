## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly (there are also alternatives to matrix inversion that we will not 
## discuss here). Your assignment is to write a pair of functions that cache the 
## inverse of a matrix.

## This function will create a Matrix and caches its Inverse

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
  ##set the Matrix to Inverse
  setInv <-function(Inv){
    m <<- inv
  }
  
  ##Get the Matrix to Inverse
  getInv <-function(){
    m
  }
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## This Function checks to see if the inverse is already calculated, is so gets it 
## from the cache and skips the computing the inverse

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    
    ## Retriving from the cache if the value of m is found in the cache
    message("getting cached data")
    ## returning the m from cache
    return(m)
  }

  data <- x$get()
  ## If m is not found in the cache then getting the reverse of the matrix
  m <- solve(data)
  x$setInv(m)
  m
}
