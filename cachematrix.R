## This file has 2 functions.
## makeCacheMatrix - creates a wrapper around a matrix, 
#helps cache matrix inverse
## cacheSolve - Computes matrix inversion if not already computed before.
#stores inverted matrix in its environment, that can be retrieved when needed
#next time

## This function, 
#1. Takes in a matrix (invertible).
#2. If not given when called, matrix can be set by set function exposed
#3. set, get, setInverse, getInverse functions are returned 
#4. get function gives the matrix stored in this environment
#5. setInverse function stores inverse of matrix into im
#6. getInverse returns inverse of the matrix stored in im.

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


## This function,
#1. Takes output of makeCacheMatrix as input
#2. Return of makeCacheMatrix is its environment (im and x), 
#list(set, get, setInverse, getInverse)
#3. Sees if the input has inverse computed, else reads matix, 
#computes inverse, and stores back in environment
#4. Returns inverse of matrix

cacheSolve <- function(x, ...) {
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

## Usage,
#1. m <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3) #Makes matrix
#2. setwd("D:/Krishna/Personal/Tutorial/Data Science JHU/R/Assignments
#/02/code/ProgrammingAssignment2/") #Sets working directory for codefile
#3. source("cachematrix.R") #Loads makeCacheMatrix, cacheSolve into memory
#4. auxMatrix <- makeCacheMatrix(m) # creates cache matrix functions
#5. cacheSolve(auxMatrix) #computes n stores inverse