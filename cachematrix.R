## R Cache Utilities for Matrices
## Provides the user with a new data structure with functionality
## to cache the inverse of the matrix once it is calculated

## makeCacheMatrix: 
## Creates a cacheMatrix: a list from an input matrix that 
##                        handles the cached data
## inputs: x: a matrix
## output: a list with four functions to get and set the stored matrix
##         and to get and set its inverse once it is calculated
## If the matrix stored changes via set, the inverse is deleted from 
## the cache
makeCacheMatrix <- function(x = matrix()) {

  inverse <-NULL
  set <- function(y) {
    x<<-y
    inverse<<-NULL
  }
  get <-function() x
  setinverse <- function(solve) inverse <<- solve 
  getinverse <- function() inverse
  list(set = set, get =get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## cacheSolve: calculates the inverse of a cacheMatrix, using  
##             the cached inverse if it was already calculated  
##             If not, then calculates the inverse with solve()
##             and stores the result in the cache
## inputs: x: a cacheMatrix
## output: the inverse of the matrix stored in the cacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
