## The following functions takes a matrix and inverses it. Since inversion of large
## matrices takes time, here, the inverse is cached and is not re-computed once it is
## calculated. This saves computing time and resources.

## The following function initialises a matrix and functions to get the value of the matrix
## A cache variable 'inverse' is created and is initialised with a value of NULL
## It then sets the value of the inverse of the matrix
## And gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(matinv) inverse <<- matinv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve function calculates the inverse of the matrix created using the above function
## If the inverse is already cached, the function gets the value of the inverse from the cache (variable 'inverse')
## Thus eliminating the need for recalculation
## If the matrix is not inversible (singular), R throws and error to this effect

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
