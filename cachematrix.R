## This file contains functions that together compute the inverse of a matrix and cache the inverse to be retrieved later

# Created by Callen Hyland
# February 25, 2022

# Function makeCacheMatrix defines a special "matrix", which is a list of functions that can get or set the value of the matrix and get or set the value of the value of the inverse that is stored in the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # function to set global variable x to matrix
  set <- function(y) {
    x <<- y
    # remove any cached value of i
    i <<- NULL
  }
  
  # function to get matrix
  get <- function() x
  
  # function to set variable i to computed inverse
  setInv <- function(inv) i <<- inv
  
  # function to retrieve cached inverse stored in i
  getInv <- function() i
  
  # combine functions into a special "matrix"
  list(set = set, get = get,
  setInv = setInv,
  getInv = getInv)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Function cacheSolve takes as input the special "matrix" defined by function makeCacheMatrix and either computes an inverse if the inverse is not cached or retrieves the cached value if it is

cacheSolve <- function(x, ...) {
  
  # get cached inverse value
  i <- x$getInv()
  # if the value is not null, send message and return the cached inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if the value of the inverse has not been cached, get the matrix
  data <- x$get()
  
  # compute the inverse of the matrix using "solve"
  i <- solve(data, ...)
  
  # and cache the value of the inverse
  x$setInv(i)
  
  # return the value of the inverse
  i
}
