## Put comments here that give an overall description of what your
## functions do
## The function makeCacheMatrix creates a vector  which contains the various functions needed in cacheSolve function
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y)
	{
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## the control flow is obvious and it first checks to see if there is a cachedInverse, if not then it calculates it.
cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m))
	{
		message("getting cached data")
		return m
	} 
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
