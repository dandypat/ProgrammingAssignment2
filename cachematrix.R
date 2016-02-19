## Dhrunal Patel - R Programming Class : Assignment 2
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##assumes that the matrix supplied is always invertible.

## Assignment

makeCacheMatrix <- function(x = matrix()) {
	## creates a special matrix 
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setsolve<-function(solve) m<<-solve
	getsolve<-function() m
	list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getsolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setsolve(m)
	m
}
