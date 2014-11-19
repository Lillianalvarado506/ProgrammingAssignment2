## makeCacheMatrix and cacheSolve work together to return the inverse of
## a matrix either from cache (if the inverse has already been computed) 
## or by computing it for the first time if it hasn't been done before.
## Using the inverse in cache allows to save on computing time.


## makeCacheMatrix is passed a matrix and creates an object of type list that 
## contains the original matrix and the cached value for its inverse, which 
## is initially set to NULL


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
		x <<- y 
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve accesses the object created by makeCacheMatrix, and returns the 
## inverse of a matrix from cache if it has already been computed. 
## If it has not been computed previously, it solves for the inverse and 
## returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
