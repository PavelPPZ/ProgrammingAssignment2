## makeCacheMatrix function creates the environment to set and get the inverse matrix
## cacheSolve calculates or extracts from cache the inverse matrix and stores it in this environment

## makeCacheMatrix: creation of the environment, definition of functions to interact with it
## x - matrix to invert. Returned "vector" of functions:
## set - define matrix to invert, get - get matrix to invert, 
## setinv - define inverse matrix, getinv - get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## set the environment to manage inverse matrix info, define functions to interact with it
        
        i <- NULL ## inverse matrix set to NULL
        
        ## function to define the matrix to inverse
        set <- function(y) {
                x <<- y ## matrix is redefined in environment
                i <<- NULL ## inverse matrix is set to NULL
        }
        
        get <- function() x ## function to get the matrix to invert
        setinv <- function(inv) i <<- inv ## function to set the inverse matrix 
        getinv <- function() i ## function to get the inverse matrix
        
        ## "vector" of functions as function output
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: calculate the inverse matrix or extract it from cache
## x - "vector" of functions to manage the environment, z - matrix to invert
## function is checking if matrix z is new or same as before and whether inverse matrix was cached
## based on this the inverse matrix is either calculated or extracted from cache

cacheSolve <- function(x,z) {
        ## Return a matrix that is the inverse of 'z'
        
        data <- x$get() ## get matrix that was inverted
        
        ## if z is different from initial matrix
        if (!identical(data,z)) {
                message("new matrix")
                x$set(z) ## matrix is redefined in environment
                m <- solve(z) ## matrix is inverted
                x$setinv(m) ## new inverse matrix is defined
                return(m)              
        }
        
        ## if z is same as initial matrix
        
        ## if inverse matrix was cached
        m <- x$getinv() ## getting inverse matrix from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if inverse matrix was not cached
        message("first calculation of inverse matrix")
        m <- solve(data) ## calculation of inverse matrix
        x$setinv(m) ## caching inverse matrix
        m
}
