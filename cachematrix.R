#Creates a matrix and caches its inverse; returns a list of functions for set/get.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = setmatrix, get = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the vector created with makeCacheMatrix(). 
## If the inverse exists, then it will retrieve it from the cache; if not, gets and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data)
        x$setinverse(m)
        m
}
