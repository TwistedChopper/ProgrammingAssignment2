## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                              ##Assigning the value of the matrix in the new environment
                x <<- y
                m <<- NULL
        }
        get <- function() x                               ##Retrieving the matrix that was just assigned in set
        setInverse <- function(solve) m <<- solve         ##Using the solve function to compute the inverse of the matrix  and assigning it a variable
        getInverse <- function() m                        ##Retrieving the inverse of the matrix that was just assigned in setInverse
        list(set = set, get = get,                        ##Returning the inversed matrix as well as the defined functions
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCahceMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {                                  ##If statement looking to see if the cached value exists and returns the cached value if it does
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                    ##If the cached value doesn't exist, then this will retrieve the matrix and calculate the inverse
        m <- solve(data, ...)                              ##and will then return the inversed matrix.
        x$setInverse(m)
        m
}