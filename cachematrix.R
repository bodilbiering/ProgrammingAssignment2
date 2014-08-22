# By Bodil Biering, 2014

## Input x is an invertible matrix, output is a list of functions to
## get and set x, and to get and set the cached inverse of x.

makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          set <- function(y){
            x <<- y
          }
          get <- function() x
          setInverse <- function(inv) inverse <<- inv
          getInverse <- function() inverse
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)

}


## Input is a list of functions as constructed by 'makeCacheMatrix'.
## Produces the inverse of the matrix stored in 'makeCacheMatrix';
## first time by solving the input matrix, and subsequently by returning
## the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inverse <- x$getInverse()
          if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
          }
          data <- x$get()
          inverse <- solve(data,...)
          x$setInverse(inverse)
          inverse
}
