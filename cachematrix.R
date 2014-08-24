## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: create a matrix object that can cache its inverse, define accessors and methods
## available methods:
## set: set the matrix to supplied value
## get: get the previously set matrix
## setinverse: set the inverse of the matrix to supplied value
## getinverse: get the previously set inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: given a matrix object returned by makeCacheMatrix, solve for its inverse
## matrix object should have had set called already
## uses previously cached solution if available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
