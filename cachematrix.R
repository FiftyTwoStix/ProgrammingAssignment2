## functions to answer Assignment 2

## MakeCacheMatrix
## ---------------
## Creates an object which contains:
##	a matrix 'x' -- the uninversed raw matrix, defaults to numeric(0)
##	a matrix 'm' -- the inverse of 'x'
##	four methods:
##		set() which assigns its parameter to 'x'
##			and initialises 'm' to NULL to indicate 'x' has changed
##		get() which returns 'x'
##		setinverse() which assigns its parameter to 'm'
##		getinverse() which return 'm'
## makeCacheMatrix returns a named list of its methods

makeCacheMatrix <- function(x = matrix()) { 
	m <- NULL					## initialises 'm'

	## the 'set' method (see above)
	set <- function(y) {
 		x <<- y				## assigns the 'set' parameter to 'x'
		m <<- NULL				## sets 'm' to NULL as 'x' has changed
	}

	## the 'get' method (see above)
	get <- function() x

	## the 'setinverse' method (see above)
	setinverse <- function(new_inverse) m <<- new_inverse	

	## the 'getinverse' method (see above)
	getinverse <- function() m

	list(set = set, get = get,		## creates a list of named methods
		setinverse = setinverse,
		getinverse = getinverse)
} 

## cacheSolve
## ----------
## Computes the inverse of a matrix
## Assumes 'x' is an object created by MakeCacheMatrix()
##
## Checks to see if inverse has already been created
##	by comparing the result of x$getinverse to NULL
##
## if x$getinverse() == NULL
##	calculate inverse matrix using solve()
##	store the result in 'x' via a call to x$setinverse()
## else
##	notify user that matrix was cached
## return the inverse matrix
##
## Assumes 'x' contains a square invertible matrix
## ... allows for further parameters to any solve() call
## Local storage:
##	m: fetched (from 'x') or calculated, inverse matrix
##	data: fetched from 'x' the uninversed matrix

cacheSolve <- function(x, ...) { 
	m <- x$getinverse()			## move the inverse matrix from 'x' into m
	if(!is.null(m)) {				## if m is not empty
		message("getting cached data")## let the user know the inverse has been cached
		return(m)				## exit function and return the cached matrix
	}
	data <- x$get()				## move the uninversed matrix from 'x' into 'data'
	m <- solve(data, ...)			## use solve() to calculate the inverse
							##    passing any additional arguments in ...
							##    and place the result into 'm'
	x$setinverse(m)				## move inverse matrix into 'x'
	m						## return the inverted matrix
} 
