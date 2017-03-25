## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invVar <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invVar <<- inverse
    getInverse <- function() invVar
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invVar <- x$getInverse()
    if (!is.null(invVar)) {
        message("Cached data")
        return(invVar)
    }
    mat <- x$get()
    invVar <- solve(mat, ...)
    x$setInverse(invVar)
    invVar
}
