## Put comments here that give an overall description of what your
## functions do

## From the assignment
  ##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  ##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
  ##  above. If the inverse has already been calculated (and the matrix has not changed), then the 
  ##  cachesolve should retrieve the inverse from the cache.

## This function creates a "matrix" -- which is really a LIST that contains functions to:
##    1.  Set the value of the matrix
##    2.  Get that value of the matrix
##    3.  Set the inverse of the matrix
##    4.  Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##  First set our container for the inverse to be NULL
  inv <- NULL
  
  ##  Now we'll create the "set" function, which sets the value of the matrix 
  ##  to whatever "y" was passed in. It also resets the inverse container "inv" to be NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##  The "get" function just reports out the matrix that was passed in
  get <- function() x
  
  ##  The "setinv" function sets the cached inverse of the matrix to be equal to the 
  ##  argument that was passed in
  setinv <- function(inverse) inv <<- inverse
  
  ##  The "getinv" function reports out the inverse of the matrix
  getinv <- function() inv
  
  ##  Creating a list with the names = thefunctions they refer to
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##  The "cacheSolve" function checks to see if there is a cached inverse. If so, it 
##  reports that; if not, it solves and caches the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##  First, capture the value of "getinv" of "x"
  inv <- x$getinv()
  
  ##  Check: inverse already calculated?
  if (!is.null(inv)) {
    
    ##  Get it from the cache
    message("Retrieving from cache")
    return(inv)
    
    }
  
  ##  If inverse is not calculated, solve the matrix stored in "x"
  mat.cache <- x$get()
  inv <- solve(mat.cache, ...)
  
  ##  Caches the resulting solution back in "x"
  x$setinv(inv)
  
  ##  Return the inverse
  return(inv)
  
}
