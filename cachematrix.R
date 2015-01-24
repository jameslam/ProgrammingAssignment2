## Together these two functions should allow for the computation of
## matrix inverses and the cacheing of repeat arguments. I attempted to
## keep the naming conventions from the original consistent, changing
## m to i and mean to inverse.

## Takes a matrix and returns a list of functions to "set" and "get"
## the matrix and its inverse, calculated using the solve() function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes a list produced by makeCacheMatrix() and returns the inverse
## either using the solve() function or the cached value.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
