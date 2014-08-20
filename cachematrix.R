## This program is designed to cache inverse matrices
## so that if they have already been calculated, the
## program will get the cached data rather than spending
## CPU cycles to perform the operation again.

## makeCacheMatrix is the constructor for the matrix
## that creates a cached matrix into the array and
## will then display it if a previously cached matrix
## is ran again.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(matrix) m <<- matrix
	getmatrix <- function() m
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve is a function that will check to see if a matrix
## has already had the inverse matrix function solved on it. If
## it has, the function will return the cached matrix inverse
## and if not, it will solve the inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setmatrix(m)
	m
}

