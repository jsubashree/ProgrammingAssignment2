## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # inve will store the cached inverse matrix
    inve <- NULL

    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        inve <<- NULL
    }
    # Get the value of the matrix
    get <- function() x

    # Set the value of the inverse
    setinve <- function(inverse) inve <<- inverse
    # Get the value of the inverse
    getinve <- function() inve

    # Return the matrix 
    list(set = set, get = get, setinve = setinve, getinve = getinve)
}



## Write a short comment describing this function

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inve <- x$getinve()

    # If the inverse has already been calculated then return it
    if (!is.null(inve)) {
        message("getting cached data")
        return(inve)
    }

    # If the inverse is not yet calculated then calculate the inverse 
    data <- x$get()
    inve <- solve(data, ...)

    # Cache the inverse
    x$setinve(inve)

    # Return it
    inve
}