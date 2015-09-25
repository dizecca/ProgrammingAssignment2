# Functions to create a holding matrix and calculate inverse only  
# when inverse has not been calculated before 
# Assumes input is already an invertible matrix

# Factory to create object to hold functions and inverse cache

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inver) inv <<- inver
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
} 

## Function to retrieve stored inverse from cache or calculate

cacheSolve <- function(x, ...) { 
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
