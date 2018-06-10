## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Assume that the inverse is initially null
  inverse <- NULL
  
  ## Set the Value of the Matrix
  set <- function(mMatrix) {
    x <<- mMatrix
    inverse <<- NULL
  }
  
  ## Return the value of the original Matrix
  get <- function() x
  
  ## Set the value of the inverse Matrix(if possible)
  setinverse <- function(minverse) inverse <<- minverse
  
  ## Return the value of the inverse Matrix(if exits)
  getinverse <- function() inverse
}


## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Local inverse variable for usage
  inverse <- x$getinverse()
  
  ## if Inverse exists return that inverse
  if(!is.null(inverse)) {
    print("returing cached value")
    return(inverse)
  }
  
  ## Local storage of Matrix
  mMatrix <- x$get()
  inverse <- solve(mMatrix)
  
  x$setinverse(inverse)
  
  return(inverse)
}
