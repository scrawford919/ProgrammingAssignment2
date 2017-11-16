## First function is to generate a special version of a matrix
## This special version has methods to set and get its value,
## as well as to set and get the inverse of itself
## These methods are stored in a list, and can be accessed
## using the $ notation (e.g., special.matrix$get())

makeCacheMatrix <- function(x = matrix) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve works in conjuction with the special
## matrix that is generated using the makeCacheMatrix function
## When called, it will first check to see if an inverse for the matrix
## already exists. If it does, it will pull this cached inverse rather
## than calculating it. If the inverse does not yet exist, it will
## be calculated using the solve() function on the matrix
## The function returns the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


# ==========
# Test Code
# ==========

# Standard matrix
test_mat <- matrix(1:4, 2, 2)
test_mat
# Standard matrix inverse
solve(test_mat)

# Create special matrix
special_mat <- makeCacheMatrix(matrix(1:4, 2, 2))
special_mat
special_mat$get()

cacheSolve(special_mat)

special_mat$getinv()
