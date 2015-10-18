##Below functions are used to create an object in which both a matrix and its inverse can be stored.
##This is useful to save computation time when inverting matrices as we can then cache the inverse of a matrix in this object and retrieve the inverted matrix without computing it again.


## The first function, makeCacheMatrix creates this object, which is in fact a list containing a function to
##	1) set the value of the matrix
##	2) get the value of the matrix
##	3) set the value of the inverted matrix
##	4) get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	## initialize the InvMat variable
	InvMat <- NULL
	## 1) function to set the value of the matrix in the object
	set <- function(y) {
		x <<- y
		InvMat <<- NULL
	}
	## 2) get the value of the matrix
	get <- function() x
	## 3) set the value of the inverted matrix in the object
	setInverse <- function(InvertedMatrix) InvMat <<- InvertedMatrix
	## 4) get the value of the inverted matrix
	getInverse <- function() InvMat
	## Create the list containing the functions defined above
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## The following function calculates the inverse of the matrix stored in the object created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverted matrix from the cache (the inverse stored in the object) and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverted matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
	## Get the value of the inverse matrix stored in the object
	InvMat <- x$getInverse()
	## Check if the inverse matrix was already computed or not (i.e. if it exists in the object)
	if(!is.null(InvMat)) {## case the inverse exists in the object
		## Display that the value is retrieved and display it
		message("Getting cached data")
		return(InvMat)
	}
	## if this part of the code is ran it is because nothing was in the cache
	## So we get the matrix from the object
	data <- x$get()
	## We inverse the matrix
	InvMat <- solve(data, ...)
	## and we store the inverse we just computed in the cache
	x$setInverse(InvMat)
	## Finally return a matrix that is the inverse of 'x'
	InvMat
}
