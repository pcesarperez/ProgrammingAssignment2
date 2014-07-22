## cachematrix.R
##
## These set of functions calculate the inverse of a matrix.
## The inverse of a matrix is a time-consuming process, so we create a
## structure to cache the inverse and recover it beyond the first calculation.


## Creates a structure wich represents a cacheable matrix.
## This structure has the ability to store the inverse of the inner matrix.
## The inverse of the matrix is cached the first time it's calculated.
## We are assuming that the matrix is always invertible.
##
## @param x Matrix object to create the cache structure.
##    By default, it's an empty matrix.
## @return A list with four functions:
##    set ( ): Setter function (initializes the structure).
##    get ( ): Getter function (retrieves the actual matrix).
##    setInverse ( ): Sets the value of the inverse of the given matrix.
##    getInverse ( ): Gets the value of the inverse of the given matrix.
makeCacheMatrix <- function (x = matrix ( )) {
    # This value holds the inverse matrix.
    i <- NULL

    ## Setter function.
    ## This function assigns a matrix to the structure.
    ## It uses lexical scoping to initialize the inner matrices, "x" and "i".
    ##
    ## @param y Invertible matrix object.
    set <- function (y) {
        x <<- y
        i <<- NULL
    }


    ## Getter function.
    ## This function retrieves the inner matrix, stored in the object "x".
    ##
    ## @return The actual matrix held into the structure.
    get <- function ( ) {
        return (x)
    }


    ## Inverse assignment.
    ## This function stores a given inverse matrix into the structure.
    ## It uses lexical scoping to initialize the inner inverse, "i".
    ##
    ## @param z The inverse of the inner matrix "x".
    setInverse <- function (z) {
        i <<- z
    }


    ## Inverse getter.
    ## This function retrieves "i", the inverse of the inner matrix "x".
    ##
    ## @return The inverse of the actual matrix held into the structure.
    getInverse <- function ( ) {
        return (i)
    }


    ## The structure returns a list with the four internal functions within.
    return (list (set=set,
                  get=get,
                  setInverse=setInverse,
                  getInverse=getInverse))
}


## Solves the inverse of a given cacheable matrix.
## If the inverse has been calculated before, the result is the cached version.
## Otherwise, the actual inverse is calculated.
##
## @param x Structure that represents a cacheable matrix.
## @return A matrix that is the inverse of the given matrix.
cacheSolve <- function (x, ...) {
    # First, we try to get the cached inverse of the given cacheable matrix.
    i <- x$getInverse ( )

    # If there is inverse matrix, we just return it (no real calculation done).
    if (!is.null (i)) {
        message ("Getting cached inverse matrix!")
        return (i)
    }

    # Otherwise, we need to calculate the inverse matrix.
    # Then, we store the inverse into the structure, for future use.
    # The inverse is finally returned.
    mat <- x$get ( )
    inverse <- solve (mat, ...)
    x$setInverse (inverse)

    return (inverse)
}