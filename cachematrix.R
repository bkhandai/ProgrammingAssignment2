## makeCacheMatrix creates a special Matrix object that can cache
## its inverse. it defines some get and set functions for the matrix
## object, and returns a list of these functions.

makeCacheMatrix <- function(x = matrix()) {
    inv_mtx  <- NULL

    set <- function(y) {
        x       <<- y
        inv_mtx <<- NULL
    }

    get <- function() {
        x
    }

    setinv <- function(im) {
        inv_mtx <<- im  ## Cache the inverse value
    }

    getinv <- function() {
        inv_mtx
    }

    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix. When the inverse of this matrix
## has not been computed earlier, cacheSolve computes the inverse and returns
## it, but also caches it for future use. If it has been computed already once,
## cacheSolve returns the cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv_mtx <- x$getinv()

    if (!is.null(inv_mtx)) {
        ## Cached value is non-NULL, hence return cached value
        message ("getting cached data")
        return (inv_mtx)
    }

    ## If we didn't return, and fell through to this point, it means that we have
    ## neither cached the inverse, or the value of the matrix has changed since we
    ## last cached it. We need to compute the new inverse.

    data      <- x$get()
    inv_mtx   <- solve(data, ...)
    x$setinv (inv_mtx)  ## This function call will do the actual caching

    inv_mtx
}
