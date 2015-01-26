## 	These two functions work in tandem to calculate and store/cache the 
## inverse of a matrix. The matrix 'x' is supplied to makeCacheMatrix,
## and an object exampleobj <- makeCacheMatrix(x) is supplied as the argument
## to cacheSolve. 
## 	cacheSolve will calculate the inverse of 'x' that was the argument to
## makeCacheMatrix, and store that inverse in cache as an element to the 
## list of objects exampleobj. If cacheSolve has already been run
## with exampleobj as the argument, it will return the inverse of 'x' from
## the cache, rather than calculating the inverse again. 


## This function creates a list of functions that cache the matrix supplied
## and the inverse of that matrix, once called by the cacheSolve function
## below.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}



## This function returns the inverse of the 'x' matrix supplied to 
## makeCacheMatrix function above.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	matrx <- x$get()
	inv <- solve(matrx, ...)
	x$setinv(inv)
	inv
}
