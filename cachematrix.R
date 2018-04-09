## This set of function allows to avoid to calculate twice the same inverse of
## a matrix by storing the inverse matrix into a cache after calculating it once

## This function creates a list for the matrix we want to inverse which 
## contains the 4 functions to set/get the matrix and to set/get the inverse 
##from the cache

makeCacheMatrix <- function(x = matrix()) {##Use the matrix you want to invert as argument
    inv <- NULL             
    set <- function(y) {    
        x <<- y
        inv <<- NULL
    }
    get <- function() x     
    setsolve <- function(solve) inv <<- solve 
    getsolve <- function() inv  
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)   ##returning a list containing the get/set(solve) functions
}


## This function will return the inverse matrix used as an argument in 
## makeCacheMatrix. If the calculation as already been done, it will recover the
##inverse matrix from the cache. If not, it will calculate the inverse and store
##it in the cache


cacheSolve <- function(x, ...) {    ##Use the result of the makeCacheMatrix as argument
    inv <- x$getsolve()     ##loading the cache for the inverse matrix
    if(!is.null(inv)) {     ##test if the inverse as already been calculated
        message("getting cached data")
        return(inv)         ##returns the inverse from the cache
    }
    data <- x$get()         
    inv <- solve(data, ...) ##calculation of the inverse
    x$setsolve(inv)         ##storing the inverse in the cache
    inv        ## Return a matrix that is the inverse of 'x'
}
