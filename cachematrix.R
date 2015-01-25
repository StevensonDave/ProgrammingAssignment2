## D Stevenson
## Coursera - R Programming, Week 3, Assignment 2
## Jan 24, 2015

## This script contains two functions:
## 1.  makeCachematrix inverts a matrix passed to the function
## and inverts it.  It Then adds the inverted matrix to the
## cache
## 2.  cacheSolve checks if an inverted matrix is already in
## the cache.  If no cached matrix, it calls makeCachematrix

## This function takes the inverse of a matrix passed to it
## with the solve function

makeCachematrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function outputs an inverted matrix
## First it checks if the cache has an inverted matrix
## If there is a matrix in the cache, that is returned
## If no cached matrix exists, the makeCachematrix function
## is called to produce the inverse


cacheSolve <- function(x) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
