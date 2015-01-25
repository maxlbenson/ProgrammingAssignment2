## cacheMatrix.R
##
## Date: 2015-01-25
##
## Contains functions to create a matrix object which
## caches the matrix inverse so it is only computed once

## This function creates an R matrix "object" that caches both the
## the raw matrix as well as its inverse
##
makeCacheMatrix <-function(rawmat = matrix()) {
	cachedinv <-NULL

	## Setter to update underlying matrix, clear cached matrix inverse
	set <- function(mat) {
		rawmat <<- mat
		cachedinv <<- NULL
	}

	## Getter to return underlying matrix
	get <- function() rawmat

	## Setter called by cacheSolve to cache matrix inverse
	setinv <- function(inv) cachedinv <<- inv

	## Getter to retrieve matrix inverse
	getinv <- function() cachedinv

	list( set = set, get = get, setinv = setinv, getinv = getinv )
}


## This function is called to calculate the inverse of a matrix object
## The first call will actually do the calculation. Subsequence calls
## will simply retrieved the cached value.
##
cacheSolve <- function(mat, ... ) {
	# try to get inverse cached in matrix object
	inv <- mat$getinv()
	if (!is.null(inv)) {
		message("using cached matrix inverse")
		return(inv)
	}

	# inverse needs to be computed - get raw matrix, compute inverse, cache result
	rawmat <- mat$get()
	inv <- solve(rawmat, ...)
	mat$setinv(inv)
	inv
}