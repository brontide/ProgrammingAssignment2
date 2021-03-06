## Programming assignment 2 by Eric Warnke 2015
## 95% of this program was based on the cache mean example
## provided by the course.

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache the
## inverse of a matrix.

# > c <- matrix(rnorm(16),4,4)             
# > c.solve <- solve(c)
# > test <- makeCacheMatrix(c)
# > test.solve <- cacheSolve(test)
# > identical(c.solve, test.solve)
# [1] TRUE
# > test.solve <- cacheSolve(test)
# getting cached data
# > identical(c.solve, test.solve)
# [1] TRUE
# > test$set(c)
# > test.solve <- cacheSolve(test)
# > identical(c.solve, test.solve)
# [1] TRUE

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(mean) m <<- mean
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        # have we solved it already
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # otherwise solve and cache result
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
