## Put comments here that give an overall description of what your
## functions do
## These functions is the implementation of The ProgrammingAssignment2 ,Caching the Inverse of a Matrix
## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(matrix){
    x <<- matrix
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## return the inverse if the matrix already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix from the object
  matrix <- x$get()
  ## cal the inverse using the solve function
  m <- solve(matrix, ...)
  ## cache the inverse of the matrix
  x$setInverse(m)
  ## return the matrix
  m
}