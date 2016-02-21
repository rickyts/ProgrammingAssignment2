## functions to avoid costly computation time to find the inverse of a matrix
## Author: Ricky T
## Date: 2016-02-21

## Summary: creates a special matrix that contains 4 functions
## Example: m <- makeCacheMatrix(matrix(1:4,ncol=2,nrow=2))
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize
    m <- NULL
    
    ## function 1
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## function 2
    get <- function() x
    
    ## function 3
    setsolve <- function(solve) m <<- solve
    
    ## function 4
    getsolve <- function() m
    
    ## return a list of 4 functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Summary: return the inverse of the special matrix makeCacheMatrix
## Example: cacheSolve(m)
cacheSolve <- function(x, ...) {
    
    ## find if the special matrix exists
    m <- x$getsolve()
    
    ## return cached answer if exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## compute invserse if does not exist
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
