## Caches the inverse of a matrix in order to save computational time

## Makes a "matrix", which is really a list containing a function to
## set and get the values of both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    set <- function(y) {
        x <<- y
        inv <<- matrix()
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse of the "matrix" created above 
## and stores it in the cache if not already calculated; 
## if in the cache already, the function gets the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.na(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
