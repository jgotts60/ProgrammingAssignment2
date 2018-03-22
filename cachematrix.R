##  The two functions below solve for the inverse of a matrix 
## and cache the inverse in a object. If the inverse already 
## exists, the cacheSolve function returns the cached inverse
## rather than computing it again. 


## This function creates an list object where each element is a 
## function to store or return the value of the matrix or the 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve 
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of the "matrix" created
## with the makeCacheMatrix function. It first tests if the 
## inverse has been cached in the list created with the 
## makeCacheMatrix function. If so, it returns that value.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}