## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function returns a list of named functions which store a matrix and its inverse. The functions set and get are the setter and getter for the matrix. The functions setinv and getinv are the setter and getter for the matrix's inverse. 
# setting the matrix resets the stored inverse because a new matrix might have a different inverse
# The default value for the inverse of the matrix (before it's been calculated) is NULL
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinv <- function(inv)  inverse <<- inv
	getinv <- function() inverse
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
# This function takes a list which stores a matrix (as created by the makeCacheMatrix above) and returns its inverse.
# If the inverse has already been calculated the cached value is returned. Otherwise the inverse is calculated and stored.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
    	return(inv)
    }
    thematrix <- x$get()
    inv <- solve(thematrix)
    x$setinv(inv)
    inv
}
