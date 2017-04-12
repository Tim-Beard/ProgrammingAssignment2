#####################################################################
## cachematrix.R
##
## These functions create a new "matrix" object that caches its own
## inverse once calculated, and returns the cached value if available
## in order to save time calculating it each time it's needed.
##
## Note that we assume the square matrix supplied is invertible
#####################################################################

## makeCacheMatrix: 
## This function creates a special "matrix" object 
## that can cache its inverse. It provides "methods" to get and set the matrix
## and to get and set (i.e. calculate and cache) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv.x <- NULL # Set the matrix inverse to NULL
    set <- function(y) { # sets the matrix contents
        x <<- y
        inv.x <<- NULL # resets matrix inverse to NULL when matrix is set
    }
    
    get <- function() x # gets the matrix contents
    setinv <- function(inv) inv.x <<- inv # caches the inverse of the matrix 
    getinv <- function() inv.x # returns inv.x (the inverse if cached or NULL)
    list(set = set, get = get, # returns a list of the 4 matrix functions
         setinv = setinv,
         getinv = getinv)
}

##########################################################################
## cacheSolve: 
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv() # gets inv.x from the special matrix object
    if(!is.null(m)) { # if a cached value exists, return it
        message("getting cached data")
        return(m)
    }
    
    data <- x$get() # if there's no cached value...
    m <- solve(data, ...) # ... calculate the inverse
    x$setinv(m) # ... and cache it for future use
    m # return the special matrix object
}
