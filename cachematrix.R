## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions makeCacheMatrix and cacheSolve are used to create a special
## object that stores a "matrix" and cache's its inverse.



## The function makeCacheMatrix creates a special "matrix" object that can cache
## its inverse. It returns a list containing a function to
## - set the matrix
## - get the matrix
## - set the inverse
## - get the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## the argument of the function is an invertible matrix
        
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        set_inv <- function(inv) inv_mat <<- inv 
        get_inv <- function() inv_mat
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
        ## this list is used as the input to cacheSolve()
}



## The function cacheSolve computes the inverse of the special "matrix" created  
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## the argument of the function is the output of the makeCacheMatrix 
        ## function
        
        inv_mat <- x$get_inv()
        
        ## first check to see if the inverse has already been calculated
        
        if(!is.null(inv_mat)) {
                # if so, it gets the inverse from the cache and skips
                ## the computation
                
                message("getting cached data")
                return(inv_mat)
        }
        
        ## otherwise, it calculates the inverse of the matrix and sets it 
        ## in the cache via the set_inv function
        
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$set_inv(inv_mat)
        inv_mat
          
}
