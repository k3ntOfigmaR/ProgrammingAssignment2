## Put comments here that give an overall description of what your
## functions do

##
## The following "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse. 
## The function takes a matrix as input (ex: matrix(c(1,8,6,8),2,2) )
## and outputs a list of methods.
## These methods allow the user to get the content of the matrix (ex: x$get()),
## or to set/modify the content of the matrix (ex: x$set(matrix(1:4, 2,2))), or
## to get the stored inverse of the matrix, or to set the externally calculated
## value of the matrix (using the cacheSolve function).
## Syntaxe: 
##
## "x" is the user provided matrix
## "inv" represents the stored inverse matrix
## "get" is the method object that outputs the matrix object
## "getinverse" is the method that outputs the calculated inverse matrix object
## "set" is the method that stores the original matrix object
## "setinverse" is the method that stores the calculated inverse matrix object
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            # Initializes the inverse matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x } 
  setinverse <- function(inverse_matrix) { inv <<- inverse_matrix }
  getinverse <- function() { inv }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##
## This function calculates the inverse of a matrix object.
## If the matrix object already had its inverse calculated and stored,
## the functioin simply outputs the stored value of the inverse matrix.
## If the inverse matrix was never calculated, the Solve function is called
## to calculate the inverse of the matrix object and the result is stored in
## the matrix object for further use.
##

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()   # Retrieves the stored inverse matrix in 'x'
  
  # If the stored matrix retrieved was previously calculated, simply output the
  # stored value.
  if(!is.null(inv)) {     
    message("getting cached data")
    return(inv)
  }
  data <- x$get()         # Retrieves the matrix from 'x'
  inv <- solve(data, ...) # Calculates the inverse of the matrix
  x$setinverse(inv)       # Stores the inverse of the matrix
  inv
}