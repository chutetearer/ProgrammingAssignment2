## This is part of assignment 2 in the R
## programming course. wjs - 2014-12-20


## makeCacheMatrix creates a "special matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## cacheSolve takes a "special matrix" as an argument and
## checks if its inverse is cached. If yes, it returns
## the cached inverse matrix, otherwise it computes the
## inverse, caches it and returns the inverse matrix.

cacheSolve <- function(x, ...) {
    i <- x$getsolve()
    if(!is.null(i)) {
        message("returning cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    message("returning calculated data")
    i
}


## test function with two different 2x2 matrices.
## To test above functions, type test() in console.

test <- function() {
    myMat1 <- matrix(c(2,2,3,2), nrow=2, ncol=2)
    print(myMat1)
    myMat2 <- matrix(c(2,3,2,2), nrow=2, ncol=2)
    print(myMat2)

    myCachMat1 <- makeCacheMatrix(myMat1)
    myInvMat1 <- cacheSolve(myCachMat1)   # calculates inverse
    print(myInvMat1)
    myCachMat2 <- makeCacheMatrix(myMat2)
    myInvMat2 <- cacheSolve(myCachMat2)   # calculates inverse
    print(myInvMat2)

    myInvMat1 <- cacheSolve(myCachMat1)   # gets cached inverse
    print(myInvMat1)
    myInvMat2 <- cacheSolve(myCachMat2)   # gets cached inverse
    print(myInvMat2)
}