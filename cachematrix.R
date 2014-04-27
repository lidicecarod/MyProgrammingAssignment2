## These functions cache the inverse of a matrix so that
## we do not need to compute it repeatedly 

## Create a matrix object providing the options of setting and 
## returning both the object and its inverse stored in cache memory

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## Compute the inverse of the object matrix "x" if the inverse has not
## been computed before

 cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
