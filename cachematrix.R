## Overall description of functions

# The first function, `makeCacheMatrix` creates a list containing functions to
# Set the value of the matrix
# Get the value of the matrix
# Set the value of the inverse
# Get the value of the inverse
#
# The second function, 'cacheSolve' calculates and stores the inverse if it
# hasn't been calculated before, or fetches it from storage, if it has.

## Short description of this function
# Creates the list of functions that set and get a matrix and its inverse.
makeCacheMatrix <- function(X = matrix()) {

    inv <- NULL
    set <- function(y) {
        X <<- y
        inv <<- NULL
    }
    get <- function() X
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Short description of this function
# Uses the list of functions created by the 1st function. Calculates and stores
# the matrix inverse if inverse has not been calculated. Fetches inverse from
# storage if it has been calculated.
cacheSolve <- function(X, ...) {
    
    inv <- X$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data, ...)
    X$setinverse(inv)
    inv
    
}
