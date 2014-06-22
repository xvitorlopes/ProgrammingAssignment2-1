## R Programming Course: Programming Assignment 2

## These R functions calculate the inverse of a matrix and store it into a 
## special "matrix" (list) cache object

## This function creates the special "matrix" (list) to cache the original 
## matrix and its calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## Create getters and setters for the special "matrix" (list) 
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## Initialize getters and setters and returns the special "matrix" 
        ## (list)
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function calculates the inverse of the matrix informed in the special 
## "matrix" l (list from the makeCacheMatrix() function). 
## If the inverse was already calculated, then it uses the value from the cache

cacheSolve <- function(l, ...) {
        
        ## If the inverse matrix was already calculated, returns it from the 
        ## cache
        
        i <- l$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- l$get()
        
        ## Return a matrix that is the inverse of 'data'
        i <- solve(data, ...)
        l$setinverse(i)
        i
        
}
