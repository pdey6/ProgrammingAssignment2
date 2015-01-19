## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## creates a special "matrix" object that can cache its inverse.
  ## set the value of the matrix
  matrixinv <- NULL
  set <- function(y) {
    x <<- y
    matrixinv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setmatrixinverse <- function(solve) matrixinv <<- solve
  getmatrixinverse <- function() matrixinv
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 
 ## get the inverse of the matrix
 matrixinv <- x$getmatrixinverse()
  
  ## check if the matrix exists
  if(!is.null(matrixinv)) {
    message("getting cached data")
    return(matrixinv)
  }
  
  ## if matrix does not exist: get the inverse of the matrix
  data <- x$get()
  matrixinv <- solve(data)
  x$setmatrixinverse(matrixinv)
  
  matrixinv
}
