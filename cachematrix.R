## makeCacheMatrix() creates a special R object which is a matrix data structure stored in the cache.
## cacheSolve() returns a matrix that is the inverse matrix stored in the cache.


## Within this special matrix object scope, set(), get(), setinverse(), and getinverse() functions are defined.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## cacheSolve() functions checks the follow:
## 1. If the inversed matrix exists in the cache, the inversed matrix is retrieved from the cache.
## 2. If the inversed matrix does not exist in the cache, the matrix is inversed by calling the
## solve() function. The inversed matrix is stored in the cache and returned.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i        
}




