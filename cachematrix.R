#!/usr/bin/Rscript
# NAME: Peter Herbert 
# FILE: cachematrix.R
# DATE: 2015-08-18
# DESC: Calculate the inverse of a matrix. If the inverse 
#       has already been calculated then retrieve the inverse.
# LAST Modified: 2015-08-22 20:01:33

# Define the functions to get and set 
# a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Setter for the matrix 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }    
    # Getter for the matrix
    get <- function() x
    # Setter for the inverse
    setinverse <- function(inverse) inv <<- inverse
    # Getter for the inverse
    getinverse <- function() inv 
    # Return the getter and setter functions
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

# Calculate or retrieve the inverse of matrix x 
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # If the inverse is already calculated return it
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    # Get the matrix x
    data <- x$get()
    # Calculate the inverse of matrix x
    inv <- solve(data) 
    # Set the invese of matrix x
    x$setinverse(inv)
    # Return the inverse
    return(inv)
}   
