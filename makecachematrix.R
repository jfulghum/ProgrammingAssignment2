## Caching the Inverse of a Martix


## These two functions create a special object that stores a matrix and cached its inverse. 

## This function creates a new matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	if(det(x)==1){
		print("Determinant must not equal 0.")
		return()
	}
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the new matrix object created by makeCacheMatrix
## If the the inverse has already been calcuated it will retreive the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        	return(inv)
        }
        data = x$get()
        m = inverse(data, ...) 
        x$setinverse(inv)
        inv
}
