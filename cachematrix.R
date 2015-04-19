## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special 'matrix', 
## which is really a list of functions:
## set - set the value of the matrix
## get - get the value of the matrix
## setinv - set the value of the inverse matrix
## getinv - get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {  // misschien nog check of matrix veranderd is?
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(x, ...)
    x$setinv(i)
    i
}
