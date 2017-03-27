## These funcstion provide the ability to cache the 
## inverse of a squared matrix.
##
## Usage:
## Create a matix and pass it to the 'makeCacheMatrix' function
## then take the returned object from this function and use it as the
## parameter passed to the 'cacheSolve' function, this will return
## the inversed version of the original matrix either from a cahced
## result or a newly solved inversion process


## This function is the primary function that provides
## the caching mechanism for a inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inv) {
        i <<- inv
    }
    getInverse <- function() {
        i
    }
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function provides the actual execution of the
## previous cached matix function

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
