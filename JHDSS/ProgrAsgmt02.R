# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# R Programming
# Coursera; Johns Hopkins Bloomberg School of Public Health
#           Baltimore, Maryland, U.S.
#
# Programming Assignment 2: Caching the Inverse of a Matrix
#           Submission Due: 22-Nov-2015 1930 ET
#
#      Author: Peter Blackmore
#       Maxim: lathe biosas
# Style Guide: https://google.github.io/styleguide/Rguide.xml
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

makeCacheMatrix <- function(mx1 = NULL) {
    # Matrix inversion is usually a costly computation and there may be some
    # benefit to caching the inverse of a matrix rather than compute it
    # repeatedly. This function creates a special "matrix" object that can
    # cache its inverse.
    #
    # Args:
    #   mx1: Matrix; A square matrix that is invertible.
    # Returns:
    #   $set(mx1)        -> the matrix
    #   $get()           -> the matrix
    #   $setInverse(imx) -> the inverse of the matrix, or NULL
    #   $getInverse()    -> the inverse of the matrix, or NULL

    mx     <- mx1
    mx.inv <- NULL

    set <- function(mx1) {
        mx.inv <<- NULL
        mx     <<- mx1
    }
    get <- function() {mx}

    setInverse <- function(imx) {mx.inv <<- imx}
    getInverse <- function()    {mx.inv}

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(mCM) {
    # Computes the inverse of a special matrix returned by the makeCacheMatrix
    # function. If the inverse has already been calculated (and the matrix
    # has not changed), then cacheSolve should retrieve the inverse from
    # the cache.
    #
    # NOTE: Non-square matrices do not have inverses; this function checks
    # that that the matrix supplied is invertible. Use the
    # matrix(sample(4), 2, 2) or matrix(sample(9), 3, 3) expression for
    # testing.
    #
    # Args:
    #   mCM: Function; Reference to the makeCacheMatrix function.

    mx <- mCM$getInverse()  # Retrieve the inverted matrix

    # If the variable is null, invert the cached matrix
    # and store it in the object.
    if (is.null(mx)) {
        mCM$setInverse(solve(mCM$get()))
    }
    # If the object already has an inverted matrix, return that matrix
    else {
        mCM$getInverse()
    }
}