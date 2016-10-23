## Pair of functions that cache the mean of a vector (Example from Coursera Assignment 3)

## This function creates a special "vector" object that can cache its mean

makeCacheVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y   #<< operator assigns value to an object that is different than current environment
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get, setmean = setmean, getmean = getmean)  #list containing functions
}


## This function computes the mean of the special "vector" returned by makeCacheVector
## If the mean has already been calculated (and the vector has not changed), then
## it should retrieve the mean from the cache

cacheMean <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {   #Checks for catched data, and gets if so
                message("getting cached data")
                return(m)
        }
        data <- x$get()   #Otherwise, gets data
        m <- mean(data, ...)   #Calculates mean
        x$setmean(m)  #Sets the value of the mean in the cache
        m
}