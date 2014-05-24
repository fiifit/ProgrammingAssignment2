## This script contains two functions 
## makeCacheMatrix creates a special matrix and caches its inverse
## cacheSolve computes the inverse of the special matrix created by makeCacheMatrix


## The makeCacheMatrix function creates a special invertible matrix which
## really is a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the matrix inverse
## 4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix created by makeCacheMatrix
## it first checks to see if the inverse has already been computed. if so
## it gets the inverse from the cache and skips the computation.
## Otherwise it computes the inverse of the matrix and sets the inverse in the 
## cache via the setinverse function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        mdata <- x$get()
        inverse <- solve(mdata)
        x$setinverse(inverse)
        inverse
}
