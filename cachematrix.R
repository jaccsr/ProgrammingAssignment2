## Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
## return a list of 4 functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}
