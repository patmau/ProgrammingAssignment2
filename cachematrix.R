## Put comments here that give an overall description of what your
## functions do

## Returns a matrix object m created from the matrix x passed as arguments.
## The following functions are available with m:
## - get()                          return the value of x
## - set(matrix(c(1,2,3,4), 2, 2))  reset x to a new value
## - getInverse()                   return the inverse matrix of x (NULL before cacheSolve function below is called )
## - setInverse(im)                 set the inverse matrix of x to im

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() x
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    getInverse <- function() inverse    # IMO, caching should be done here...
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes a matrix object x created by makeCacheMatrix above and returns the (cached) inverse Matrix.
## NOTE: Assumes that the matrix actually IS inververtible. 

cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) {
        print("getting cached inverse")
        inverse
    }
    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
