# makeCacheMatrix() creates an object cache of a matrix in order to sidestep reiteration,
# which can be computationally costly.
makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) a <<- solve
        getinverse <- function() a
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() returns the inverse of a matrix that was saved into a list cache using
## the function makeCacheMatrix() above. 

cacheSolve <- function(x, ...) {
        a <- x$getinverse()
        if(!is.null(a)) {
                message("Getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        a
}