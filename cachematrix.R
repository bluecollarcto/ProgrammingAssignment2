## Takes a matrix and stores a matrix and caches its inverse
## This can help in reducing time by not recomputing the inverse
## NOTE: there is a message function in the cachesolve function
## that outputs when an inverse is cached
## This will not affect assignment of cacheSolve to a variable

## creates a list of functions to set the value of a matrix,
## get the value of a matrix, set the inverse of the matrix,
## and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ##pass in a matrix 
    inv<-NULL  # set inv variable to NULL to start
    #set value of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get value of matrix
    get <- function() x  #return matrix
    #setup function to  solve for inverse
    setinv <- function(my_inverse) inv <<- my_inverse
    getinv <- function() inv
    #returns list of functions for set, get, setinv, and getinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Checks if we have a cached version of the the inverse of the matrix
## If we have one - it returns
## Otherwise is calculates using the solve() function
## This will cache it for future queries of the same matrix (if no changes)
## And then return the inverse of the matrix passed to it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached inverse")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
