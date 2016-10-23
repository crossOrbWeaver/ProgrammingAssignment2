## Write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y   #<< operator assigns value to an object that is different than current environment
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  #list containing functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then
## it should retrieve the inverse from the cache
# It assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {   #Checks for catched data, and gets if so
                message("getting cached data")
                return(inv)
        }
        data <- x$get()   #Otherwise, gets data
        inv <- solve(data, ...)   #Calculates inverse
        x$setinverse(inv)  #Sets the value of the inverse in the cache
        inv
}
