## These functions create a special matrix and cache the value for later use.
## If the value is already cached it returns that value.  Otherwise it creates
## a new cached value.
## example1: a<-makeCacheMatrix(matrix(1:4,2,2))

## makeCacheMatrix creates a special matrix and provides functions to cache its value.
## example2: a<-makeCacheMatrix()
## you can use the set function of the "a" object to set the matrix value. Or see example1.
## example2: a$set(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inv) m <<- inv
        getmatrix <- function() m
        list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## cacheSolve takes a special matrix object created with makeCacheMatrix, created its inversion and 
## stores it in cache.  If you rerun the same object it will put it from the cache. 
## if you define a new object it will create the inversion and store the new value in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
  
}
