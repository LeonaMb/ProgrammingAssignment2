## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## the matrix supplied must always be invertible

makeCacheMatrix <- function(x = matrix()) {
        m <<- NULL
        setmatrix <- function(y) { 
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() {x}
        getinv <- function() {m}
        setinv <- function(inv) { m <<- inv}
        list(getmatrix = getmatrix, setmatrix = setmatrix, getinv = getinv, setinv = setinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        
        m <- solve(data, ...)
        x$setinv(m)
        m
}
