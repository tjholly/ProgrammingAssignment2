## Author: Tom Holly
## Date: 25 Jan 2015
## File: Contains functions makeCacheMatrix and cacheSolve
##       that creates the inverse of a matrix using caching.
## Usage/Example: 
##   CM = makeCacheMatrix(matrix(1:4, 2,2))
##   CMI = cacheSolve( CM )

## Function: makeCacheMatrix
## Purpose:  creates a special "matrix" object that can 
## cache its inverse.
## This function mimics the example function makeVector

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){ 
                x <<- y
                m <<-NULL
        }
        get <- function() x
        setInverse <- function(mat) m<<-mat
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function: cacheSolve
## Purpose:  computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve 
## the inverse from the cache
## This function mimics the example function cacheMean
## TODO: Could check the input type to ensure it is a square matrix,
##      and is invertible.  These are assumed at the moment
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        ## Return a matrix that is the inverse of 'x'
        theInverse <- solve(data,...)
        x$setInverse(theInverse)
        theInverse
}
