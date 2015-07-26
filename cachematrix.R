## Put comments here that give an overall description of what your
## functions do:
## Write a short comment describing this function:
## there are two functions that cache and compute inverse of a matrix.
## This (1) creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## Write a short comment describing this function:
## This (2) calculate the inverse of the special matrix
## returned by `makeCacheMatrix` in (1). If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
