## The following functions create a special matrix that
## can cache a calculation of that matrix's inverse. 

## This function makes a special "matrix" that is really 
## a list containing a function to make the value of a matrix,
## get the value of a matrix, solve the value of the matrix's 
## inverse, and get the value of a matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve(x)
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function solves for the inverse of a given invertible square matrix,
## but first it checks to see if this operation has already been done. 
## If so, it gets the value from the cache.

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
}
