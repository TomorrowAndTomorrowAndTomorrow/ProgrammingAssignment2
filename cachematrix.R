## As the calculation of a matrix inverse can be costly, there may be some
## benefit to using function closures to caching the inverse of a matrix - once
## computed - rather than computing the inverse for a particular matrix repeatedly.
## The following two functions together implement the calculation - and caching
## of the inverse of a matrix (which is assumed to be invertible).

## This function encapsulates (1) the matrix to be inverted, (2) [a variable
## carrying the inverse matrix, and (3) functions which allow another function to
## access or alter either or both of the inputted matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set_fn <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get_fn <- function() x
        setinverse_fn <- function(my_inverse) inv <<- my_inverse
        getinverse_fn <- function() inv
        list(set = set_fn, get = get_fn,
             setinverse = setinverse_fn,
             getinverse = getinverse_fn)
}

## This funcion returns the inverse of the matrix cached in an object instantiated
## by the makeCacheMatrix function.  The cacheSolve function achieves this either
## by directly computing the inverse, or checking whether the inverse for the
## matrix has already been computed and (if so) returns that previously computed
## inverse matrix.

cacheSolve <- function(x) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        my_matrix <- x$get()
        inv <- solve(my_matrix)
        x$setinverse(inv)
        inv
}
