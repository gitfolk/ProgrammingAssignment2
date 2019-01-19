## This file contains two functions which address the potential costly computation 
## of inverse matrices by enabling a cache to store the inverse of a given matrix
## after it's intial computation


## *****FUNCTION makeCacheMatrix***** 
##  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## first check to see if x is a matrix
    if(!identical(class(x), "matrix")){
        message("Input not compatible.  Make sure input is a matrix")
        return(NA)
    }
    ## we need to set the matrix x and inverse n
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) n <<- inv
    getinverse <- function() n
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## *****FUNCTION cacheSolve*****
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
## Function will take a list object generated from 
## Assumptions
##  *all provided matricies are inversible
cacheSolve <- function(x, ...) {
    ## First check to see if x is a 'special' matrix - not a bulletproof test, but enough for this exercise
    if(!identical(names(x),c("set","get","setinverse","getinverse"))){
        message("Input not compatible, run makeCacheMatrix on your input first")
        return(NA)
    }
    
    ## Return a matrix that is the inverse of 'x'
    ## Let n be the inverse matrix
    ## Check if n is cached
    n <- x$getinverse()
    if(!is.null(n)) {
        message("getting cached data")
        return(n)
    }
    ## Get the matrix m
    m <- x$get()
    ## Set the identity matrix i
    i <- diag(nrow(m))
    ## Get the inverse n using the solve function
    ## Assuming always inversible so no error handling
    n <- solve(m,i)
    ## Set the inverse of x
    x$setinverse(n)
    n
}
