## cachematrix.R
## Author: egodbole
## Date: 2020-04-08

## -----------------


##  The aim of these functions is to store the value of the inverse of a matrix
##  in a cached variable so that (provided the matrix is the same) the inverse
##  of the matrix need not be computed from scratch every time it is called.

## the script cachematrix.R contains 2 functions:
##      1) makeCacheMatrix
##      2) cacheSolve

## makeCacheMatrix creates a special matrix object with four behaviours:
##     1) set matrix
##     2) get matrix
##     3) set matrix inverse
##     4) get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialises the "inv" variable
    inv <- NULL
    
    # initialises "inv" and "x" on assignment of new matrix "x"
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # gets the matrix "x"
    get <- function() {
        x
    }
    
    # assigns the inverse of matrix "x" as "inv" in its parent environment
    setinv <- function(inverse) {
        inv <<- inverse
    }
    
    # gets the inverse of matrix "x"
    getinv <- function() {
        inv
    }
    
    # returns the result in the form of a named list
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve gets the actual matrix from the object made by makeCacheMatrix,
## computes the inverse of the matrix and sets the inverse into the special
## matrix object.

cacheSolve <- function(x, ...) {
    ## assigns current value of inverse to "inv"
    inv <- x$getinv()
    
    ## checks if "inv" has a value or not
    ##    - if "inv" is not assigned a value, inv=NULL and loop is skipped
    ##    - if "inv" is assigned a value, the loop is entered
    if(!is.null(inv)) {
        # message
        message("getting cached data")
        
        # returns value of "inv" and exits the function
        return(inv)
    }
    
    # computes inverse of matrix and assigns it to "inv" to the special matrix
    # object created by makeCacheMatrix
    
    # assigns the matrix to data
    data <- x$get()
    # computes the inverse of matrix and assigns it to "inv"
    inv <- solve(data, ...)
    # assigns value of "inv" to the special matrix object
    x$setinv(inv)
    # auto-prints the value of "inv"
    inv
}