## cachematrix.R, by Adam J Heller. April 5th, 2016.
##
##
## The purpose of this assignment is twofold:
## 
## 1) to write a function that creates a matrix "object" that maintains a cache 
## of its inverse for efficienty of repeated inverse calculations, and
## 
## 2) to write a caching inverse calculator function that utilizes this
## cacheMatrix's caching abilities
## 
##
## Note: it is assumed (by instruction) that the given matrix will always be 
## invertible. If the matrix is not invertible (e.g. matrix(1:16,4,4)), an error
## will be thrown from the cacheSolve function.
## 
## [For programming assignment #2, of Johns Hopkins University's R Programming 
## course (through Coursera).]



## makeCacheMatrix takes a standard matrix object and returns a version of it 
## that can maintain a cache of its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## TODO for robustness: ensure x is a matrix, and ensure it's a square matrix
  i <- NULL
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve will either return the cached value of the matrix's inverse if it 
## has already been computed, or it will compute that inverse and store it in 
## the cacheMatrix's cache for next time.
## 
## Throws an error if the matrix is singular.

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(x$get())
  x$setinverse(i)
  i
}
