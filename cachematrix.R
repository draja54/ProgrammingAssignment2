## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix function does the following.
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() 
        x
    
    setInv <- function(Inverse) 
        inv <<- Inverse
    
    getInv <- function()
        inv
    
    list(set = set, 
         get = get, 
         setInv = setInv, 
         getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inv <- x$getInv()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data,...)
    
    x$setInv(inv)
    
    inv
}
