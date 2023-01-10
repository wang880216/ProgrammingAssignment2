## These functions are to store a matrix and cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = getInverse,
             getInverse = setInverse)
}


## The function of cachSlove is retireve the inverse from cache if invers hass been calculated

cacheSolve <- function(x, ...) {
      
        inv <- x$getInverse ()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv
}
