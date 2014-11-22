## Put comments here that give an overall description of what your
## functions do: These functions use a matrix as argument to create the inverse of said matrix. 
## The functions use the advantage of caching the inverse result to avoid doing the inverse computation
## each time this is requested.

## This function creates a matrix and has additional sub-functions set/get to overwrite, retrieve the
## matrix, as well as setinverse/getinverse for the inverted version (obtained via "solve").

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverted matrix, but first it checks if this has already been done
## using the "if" for parameter "m." In case the inverse is already computed, a message stating this
## is shown. Otherwise, "data" is written via get() and solve is used. No message indicating this is 
## cached data is shown.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
