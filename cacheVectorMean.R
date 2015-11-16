# Creates or coerces the object of type "numeric"a double-precision vector of the specified length with each
# element equal to 0
# Creates a special "vector", which is really a list containing a function to
#  1. Set the value of the vector
#  2. Get the value of the vector
#  3. Set the value of the mean
#  4. Get the value of the mean
#
makeVector <- function(x = numeric()) {
    # The operators <<- and ->> are normally only used in functions, and cause
    # a search to made through parent environments for an existing definition
    # of the variable being assigned. If such a variable is found (and its
    # binding is not locked) then its value is redefined, otherwise assignment
    # takes place in the global environment. 
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}