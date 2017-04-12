## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv.x <- NULL # Set the matrix inverse to NULL
    set <- function(y) {
        x <<- y
        inv.x <<- NULL # reset matrix inverse to NULL when matrix is set
    }
    get <- function() x
    setinv <- function(solve) inv.x <<- solve
    getinv <- function() inv.x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
