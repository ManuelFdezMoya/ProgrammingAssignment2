## A pair of functions that cache the inverse of a matrix
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## I've adopted the algorithim used in the example, and checked that works ok


## This function creates several functions to handle object 
## that can cache its inverse
### 1. "set" to set matrix to do the inverse
### 2. "get" to print the matrix that has been set
### 3. "setinverse" to set the inverted matrix
### 4. "getinverse" to print then inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if inversed matrix has been previously computed, and
## get the inverse in that case. If not: computes the solution. 
## Ends with: "setinverse", and printing the solution
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
 

# EXAMPLE OF USE
# a <- makeCacheMatrix()           
# tf<-matrix(c(1,10,3,7,0,11,12,13,0), nrow = 3, ncol=3, byrow=TRUE)
# a$set(tf)
# cacheSolve(a)   
