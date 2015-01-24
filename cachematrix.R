## This script contains two functions - 
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## These functions closely match the example functions. Each function has a
## description. The following is a paste of the output of the function calls
##> x = matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> mymat <- makeCacheMatrix(x)
##> cacheSolve(mymat)
##[,1] [,2]                        These lines are from 
##[1,]    1    3                   the print statement
##[2,]    2    4                   that I commented out
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(mymat)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## This function creates a special matrix object, which is really a list 
## containing a function to: set the value of the matrix; get the value of 
## the matrix; set the value of the inverse of the matrix; get the value of 
## the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## This function calculates the inverse of the special matrix object created 
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
#  print(data)
  m <- solve(data)
  x$setinv(m)
  m  
}