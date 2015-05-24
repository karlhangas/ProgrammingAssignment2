##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## These two functions serve as cache to store the inverse of a matrix generated
## by using the R function 'solve'.
##

##
## makeCacheMatrix:
## Creates a special 'matrix' object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
     # initialize matrix variable
     inv <- NULL          
     # set the value of matrix to what it is provided by argument
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     # get the current value of the matrix
     get <- function() x
     # set the matrix with its inverse
     setinverse <- function(solve) inv <<- solve
     # get the inverse matrix
     getinverse <- function() inv
     # return vector type list of functions
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

##
## cacheSolve:
## Computes the inverse of the special 'matrix' returned by makeCacheMatrix
## Return a matrix that is the inverse of 'x'
##
cacheSolve <- function(x, ...) {
     # get the inverse from the matrix supplied
     inv <- x$getinverse()
     # if the inverse was cached return it along with a message
     if(!is.null(inv)) {
          message("getting cached inverse")
          return(inv)
     }
     # otherwise store the data in 'data' variable
     data <- x$get()
     # compute the inverse of the data
     inv <- solve(data, ...)
     # call function to cache the inverse
     x$setinverse(inv)
     # return the inverse
     inv
}
