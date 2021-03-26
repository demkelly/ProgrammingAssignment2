## cachematrix.R contains two functions intended to invert matrices and cache the values
## of the inverse.

## makeCacheMatrix makes an R object which holds an invertible matrix and its inverse
## Returns a list of four functions( set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(z) inv <<- z
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve takes a makeCacheMatrix object and attempts to find a cached value for
## the inverse, otherwise it solves for the inverse and stores it using setinverse from
## the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message('getting cached data')
    return(invx)
  }
  mat <- x$get()
  invx <- solve(mat,...)
  x$setinverse(invx)
  invx
}
