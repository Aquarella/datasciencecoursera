## These functions compute and cash the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setmi <- function(solve) mi <<- solve
        getmi <- function() mi
        list(set = set, get = get, setmi = setmi, getmi = getmi)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        mi <- x$getmi()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setmi(mi)
        mi
}
