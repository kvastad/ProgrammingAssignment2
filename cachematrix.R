## Writing a script with the purpose of storing data for time consuming calculation, so that the calculation of the same matrix does not need to be done more than once. 

## Creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Function that computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse has already been calculated, then cacheSolve retrives the inverse from the cache. 

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
