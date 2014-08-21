## The functions below implement a caching functionality for finding the inverse
## of a matrix and caching it, so that other calls don't need to recalculate the inverse
## but simply retrieve it from the cache


## create a cache object that stores a matrix, as well as functions for:
## Setting the matrix
## Getting the matrix
## Setting the inverse of the matrix
## Getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
      
}


## Attempts to find the inverse of the matrix and store it in the cache
## for subsequent calls 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){  ##inverse stored in cache, so return cached data
                message("getting cached data")
                return (i)
        }
        
        #inverse not stored in cache, so we need to calculate it, store it in cache and return it
        data <- x$get()
        i <- solve (data, ...)
        x$setinverse(i)
        i
}
