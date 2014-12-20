## This is part of assignment 2 in the R
## programming course. wjs - 2014-12-20

## This file contains three functions related to
## inverting a matrix and keeping the inverse matrix
## cached so that it can be retrieved quickly.
##
## makeCacheMatrix() converts a matrix into a
## "special matrix". Its sub-functions include:
## get(), set(), getsolve() and setsolve().
##
## cacheSolve() either inverts a matrix and caches
## it for future use, or returns the cached inverse
## matrix if it has been previously computed.
##
## test() allows for rapid validation of the two
## previously described functions.



## makeCacheMatrix() creates a "special matrix"
## object that can cache its inverse. The
## function needs an invertible matrix as
## parameter and returns the matrix in a special
## form that can be handled by cacheSolve().

makeCacheMatrix <- function(mat = matrix()) {
    inverted <- NULL

    set <- function(rawMat) {
        mat <<- rawMat
        inverted <<- NULL
    }
    
    get <- function() {
        mat
    }

    setsolve <- function(solved) {
        inverted <<- solved
    }
    
    getsolve <- function() {
        inverted
    }
    
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}



## cacheSolve() takes a "special matrix" as an argument and
## checks if its inverse is cached. If yes, it returns
## the cached inverse matrix, otherwise it computes the
## inverse, caches it and returns the inverse matrix.

cacheSolve <- function(mat, ...) {
    inverted <- mat$getsolve()

    if(!is.null(inverted)) {
        message("returning cached inverse")
        return(inverted)
    }

    data <- mat$get()
    inverted <- solve(data, ...)
    mat$setsolve(inverted)
    message("returning calculated inverse")
    inverted
}



## test function with two different 2x2 matrices.
## To test above functions, type test() in console.

test <- function() {
    myMat1 <- matrix(c(2,2,3,2), nrow=2, ncol=2)
    print(myMat1)
    myMat2 <- matrix(c(2,3,2,2), nrow=2, ncol=2)
    print(myMat2)

    myCachMat1 <- makeCacheMatrix(myMat1)
    myCachMat2 <- makeCacheMatrix(myMat2)

    myInvMat1 <- cacheSolve(myCachMat1)   # calculates inverse
    print(myInvMat1)
    myInvMat2 <- cacheSolve(myCachMat2)   # calculates inverse
    print(myInvMat2)

    myInvMat1 <- cacheSolve(myCachMat1)   # gets cached inverse
    print(myInvMat1)
    myInvMat2 <- cacheSolve(myCachMat2)   # gets cached inverse
    print(myInvMat2)
}
