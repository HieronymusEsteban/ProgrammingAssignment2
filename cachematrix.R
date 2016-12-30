## The objective of the code below is to automatically invert any invertible matrix and to cache the inverted matrix such that the cached result (the inverted matrix) can be retrieved if needed. This is helpful if the same matrix has to be inverted several times: The inversion only has to be executed once and then the cached result can be retrieved. 


## The below function ('makeCacheMatrix') creates a list of four functions and takes a matrix as an argument. This matrix will be stored inside this same list and can be printed by evoking one of the four functions in the list ('get()'). The four functions stored in the list created by 'makeCacheMatrix' are concerned with storing and retrieving input data (a matrix that is accepted as an argument by 'makeCacheMatrix') and with storing, erasing and retrieving the output of the 'cacheSolve' function, which inverts matrices (see description below). The four functions perform the following tasks: 'set': takes a new matrix as an argument and stores it in the list and makes sure that previously stored inverted matrices (produced by 'cacheSolve') are erased. 'get': retrieves the matrix stored in the list. 'set.inverse' stores the new results produced by 'cacheSolve' (i.e. the inverse of the matrix) in the list. 'get.inverse' retrieves the obtained result (i.e. the inverse of the matrix). 


makeCacheMatrix <- function(x = matrix()) {
	x.inverse <- NULL
	set <- function(y){
		x <<- y
		x.inverse <<- NULL
		}
	get <- function() x
	set.inverse <- function(inverted.Matrix) x.inverse <<- inverted.Matrix
	get.inverse <- function() x.inverse
	list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## The below function ('cacheSolve') accepts the output of 'makeCacheMatrix' as an input (x) and can invert the matrix stored in x. 'cacheSolve' first checks if there is already an inverted matrix (x.inverse) stored in x and executes a new matrix inversion only if there is no previously inverted matrix stored in x. If it finds an inverted matrix stored in x, it will retrieve and return this previously stored inverted matrix. 

cacheSolve <- function(x, ...) {
	x.inverse <- x$get.inverse()
	if(!is.null(x.inverse)){
		message("getting cached data")
		return(x.inverse)
	}
	data <- x$get()
	x.inverse <- solve(data, ...)
	x$set.inverse(x.inverse)
	x.inverse
        ## Return a matrix that is the inverse of 'x'
}
