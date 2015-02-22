## Thoses functions creates a special "matrix" object storing the matrix
##	and its inverse (if calculated at least one time) and restore its inverse if
##	futher re-calculation is asked.
##
##	The matrix should be store in a new object using the function makeCacheMatrix()
##	as newMatrix <- makeCacheMatrix(matrix)
## 	and inverse can be calculated or obtain using the function
##	cacheSolve(newMatrix).
##
##
## example:
## --------
## 	In this example we show a typical case of use of the functions 
##		makeCacheMatrix() and cacheSolve()
##
## Here we build a square Matrix (x)
##	
##> x <- stats::rnorm(16)
##> dim(x) <- c(4,4)
##> x
##[,1]       [,2]       [,3]       [,4]
##[1,]  1.72217038  0.2740427 -0.4090343  0.2861305
##[2,]  0.05641769 -0.3610035 -0.2356725  1.5717658
##[3,] -1.84614573  1.3250853  0.9297387 -0.8355489
##[4,]  0.22937111  0.1135067 -0.5113745 -0.1866716
##
## When build a new matrix object containing the matrix x.
##
##> newM <- makeCacheMatrix(x)
##
## For the first calculation of inverse of the matrix
##	stored in newM, the inverse is calculated with the
##	function solve.
##
##> cacheSolve(newM)
##[,1]       [,2]        [,3]       [,4]
##[1,] 0.5272583 -0.2265384 -0.12423007 -0.5432015
##[2,] 0.6232713  0.3044524  0.66056120  0.5621300
##[3,] 0.3123929 -0.2794786  0.03212953 -2.0181726
##[4,] 0.1710680  0.6723799  0.16099455 -0.1539992
##
## The following time of the 
##
##> cacheSolve(newM)
##getting cached data
##[,1]       [,2]        [,3]       [,4]
##[1,] 0.5272583 -0.2265384 -0.12423007 -0.5432015
##[2,] 0.6232713  0.3044524  0.66056120  0.5621300
##[3,] 0.3123929 -0.2794786  0.03212953 -2.0181726
##[4,] 0.1710680  0.6723799  0.16099455 -0.1539992



## This function creates a special "matrix" object that store
##	the matrix giving as argument and its inverse if calculate
##	with cacheSolve.
##

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
			setsolve = setsolve,
			getsolve = getsolve)
}


## Write a short comment describing this function
##	(if calculated at least one time) and restore its inverse (if
##	re-calculated is more than one time).

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
