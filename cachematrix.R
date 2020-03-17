## Put comments here that give an overall description of what your
## functions do

## This makeCacheMatrix receives an invertible matrix and cache it
## There are 4 embedded subfunctions defined: set, get, setinvm and getinvm

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvm <- function(solve) m <<- solve
        getinvm <- function() m
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}
## cachesolve calculate the inverted matrix if has not done before
## if the inverted matrix was calculated, it just returns the cached matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvm()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvm(m)
        m
}
