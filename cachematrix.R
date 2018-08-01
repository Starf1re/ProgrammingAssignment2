## The below functions cache the inverse of a user inputted matrix

## Since matrix inversion can be a costly computation it is beneficial to cache 
## the inverse of a matrix rather than to compute it repeatedly.
## The following pair of functions create a special object in order to store a 
## matrix and then cache its inverse until a new matrix is inputted by the user 
## in which case the cache is flushed and set to NULL to store the new value for
## inverse.

## The first function, makeCacheMatrix creates a special "matrix" object, which 
##is really a list containing functions to set and get the matrix and also set 
## and get the inverse of that matrix (set being to cache the inverse).


makeCacheMatrix <- function(x = matrix()) {

        inverse <-NULL
        
        set <- function(y) {
                x<<-y
                inverse<<-NULL
        }
        
        get <- function() x
        setInverse <- function(solve) inverse<<- solve
        getInverse <- function() inverse
        
        list (set = set, get=get, setInverse = setInverse, 
              getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" that
## has been created by the above makeCacheMatrix function utillising the solve()
## function. In the case the inverse has already been calculated and the input 
## matrix (x) is unchanged,it would just retrieve the inverse from the cache and
## not recalculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInverse()
        
        if(!is.null(inverse)) {
                
                message("Retrieving cached data")
                return(inverse)
        }
        
        input <- x$get()
        inverse <-solve(input, ...)
        x$setInverse(inverse)
        inverse	 
}
