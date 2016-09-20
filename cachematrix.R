## The functions below are used to create a special object that  
## stores a matrix and caches its inverse. Matrix inversion can be 
## a costly computation so there may be benefit to caching the 
## inverse rather than computing it repeatedly.

## This function creates an R object that stores a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x 
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)		
}


## This function calculates the inverse of the special matrix with 
## makeCacheMatrix. If the inverse has been calculated, it gets the 
## inverse from the cache and skips computation. Otherwise, it calculates  
## the inverse and sets the value in the cache via setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	mat <- x$get()
	i <- solve(mat, ...)
	x$setinverse(i)
	i
}
