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

makeCacheMatrix <- function(x = matrix()) {
    # Matrix inversion is usually a costly computation and there may be some
    # benefit to caching the inverse of a matrix rather than compute it
    # repeatedly. This function creates a special "matrix" object that can
    # cache its inverse.
    #
    # Args:
    #   x: Matrix; A square matrix that is invertible.
    #              Can be a 1x1 matrix with NA as single element.
    # Returns:
    #   $set(x)        -> the matrix
    #   $get()         -> the matrix
    #   $setInverse(x) -> the inverse of the matrix, or NULL
    #   $getInverse()  -> the inverse of the matrix, or NULL
    
    mx     <- x
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

cacheSolve <- function(x, ...) {
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
    
    mx <- x$getInverse()  # Retrieve the inverted matrix
    
    # If the variable is null, invert the cached matrix
    # and store it in the object.
    if (is.null(mx)) {
        x$setInverse(solve(x$get(), ...))
    }
    # If the object already has an inverted matrix, return that matrix
    else {
        x$getInverse()
    }
}

# *****************************************************************************
# TEST SCRIPT(s)
# Use the following to test the makeCacheMatrix and cacheSolve functions
#
# NOTE: The following uses a vector of random permutation of the elements.
#       Your matrices may contain different elements.
# -----------------------------------------------------------------------------
#
# > z1 <- matrix(sample(9), 3, 3)
# > f <- makeCacheMatrix(z1)
# > f$get()
#      [,1] [,2] [,3]
# [1,]    5    3    1
# [2,]    7    9    2
# [3,]    4    8    6
# > f$getInverse()
# NULL
# > cacheSolve(f)
# > f$getInverse()
#            [,1]        [,2]        [,3]
# [1,]  0.3518519 -0.09259259 -0.02777778
# [2,] -0.3148148  0.24074074 -0.02777778
# [3,]  0.1851852 -0.25925926  0.22222222
# > z1
#      [,1] [,2] [,3]
# [1,]    5    3    1
# [2,]    7    9    2
# [3,]    4    8    6
# > y1 <- solve(z1)
# > y1
#            [,1]        [,2]        [,3]
# [1,]  0.3518519 -0.09259259 -0.02777778
# [2,] -0.3148148  0.24074074 -0.02777778
# [3,]  0.1851852 -0.25925926  0.22222222
# > f <- makeCacheMatrix()
# > f$get()
#      [,1]
# [1,]   NA
# > f$getInverse()
# NULL
# > z2 <- matrix(sample(9), 3, 3)
# > z2
#      [,1] [,2] [,3]
# [1,]    4    7    8
# [2,]    6    5    1
# [3,]    2    3    9
# > f$set(z2)
# > f$get()
#      [,1] [,2] [,3]
# [1,]    4    7    8
# [2,]    6    5    1
# [3,]    2    3    9
# > cacheSolve(f)
# > f$getInverse()
#             [,1]        [,2]       [,3]
# [1,] -0.31818182  0.29545455  0.2500000
# [2,]  0.39393939 -0.15151515 -0.3333333
# [3,] -0.06060606 -0.01515152  0.1666667

# --< end of file >------------------------------------------------------------
