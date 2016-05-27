## Put comments here that give an overall description of what your
## functions do

## These functions provide for the storing & retrieving of the inverse of a
## matrix. lexical scoping rules are utilized to effect the cacheing.Cacheing is
## critical to ensuring scarce computational resources are not wasted 
## re-computing the same information which is much more efficiently stored 
## and easily retrived


## makeCacheMatrix: Per the instructions I've created an "matrix" object
##capable of caching its inverse.


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


##cacheSolve provides the inverse of matrix stored in the object 
## created by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}

