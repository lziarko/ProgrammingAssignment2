## My functions are designed to caching the inverse of a given matrix. As a calculating
## of the inversion of the matrix could be an expensive procedure (in terms of time and resources,
## especially when matrix or matrices are large and computations are made repeatedly), my functions
## allows to store the result from the initial calculation, and, as long as elements of a given matrix
## does not change, subsequent calculations are not required / needed.

## makeCacheMatrix functions:
## 1. setting the empty matrix to store the result - the inverse of the matrix
## 2. setting and getting the matrix and the inverse of it

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks whether the inverse of the matrix has been already calculated.
## If so, it returns the cached result, if not, it runs the needed calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
