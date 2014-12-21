## Put comments here that give an overall description of what your
## functions do
## These two functions allow the storage of matrices and their inverses for recall later
## avoiding the need to recalculate the inverse

## Write a short comment describing this function
## creates a list that stores matrix inputs and inverses once calculated
## provides functions for storage and recall of both matrix and inverse
## clears previously calculated inverse if a new matrix is stored

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function () x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## this function takes a list created from the makeCacheMatrix function 
## and returns a cached matrix inverse if it already exists
## if not, it calculates, caches, and returns the inverse.

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinv(i)
	i
}
