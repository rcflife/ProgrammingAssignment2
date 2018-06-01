makeCascheMatrix <- function(x = matrix()) {
      ## makes a unique "martix" object which can cache its inverse  
	## sets the value of the Matrix
	## gets the value of the Martix
	## sets the value of the inverse
	## gets the value of the inverse

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse ## sets the cache(m) equal
	## to the inverse matrix(x)
        getinverse <- function() m ##the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cachesolve <- function(x, ...) {
        ## computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been found, then it retrieves the inverse from the cache.
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
