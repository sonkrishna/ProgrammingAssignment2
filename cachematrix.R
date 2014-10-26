## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  set <- function(mat) {
    x <<- mat
    im <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) im <<- inv
  
  getInverse <- function() im
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getInverse()
  
  if(!is.null(mi)) {
    message("Getting inverse")
    return(mi)
  }
  
  data <- x$get()
  mi <- solve(data, ...)
  x$setInverse(mi)
  
  mi
}
