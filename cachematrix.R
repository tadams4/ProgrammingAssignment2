## These functions create a special "matrix" object that can cache its inverse and compute 
## the inverse.  If the inverse has already been calculated and cached within the first
## function, the second function will retrieve the inverse in the cache.
## If not, the second function will grow through the calculation of the inverse and provide it.

## The makeCacheMatrix function will take a matrix it is given (assuming it has an inverse),
## assign it to a variable, and clears the cache.  When you refer to the matrix again, 
## it can also get this matrix for you, calculate its inverse for you,
## and save it in the cache, if you run the makeCacheMatrix subfunctions.  

makeCacheMatrix <- function(x = matrix()) {
	invm <- NULL
	set <- function(y) {
		x <<- y
		invm <<- NULL
	}
	get <- function() x
	setinverse <- function(solve)invm <<-solve
	getinverse <- function() invm
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function will, when given the variable name of the matrix that was
## passed to the makeCacheMatrix,
## take the inverse that has been stored in the previous function and provide a print out
## of the inverse.  If there is no inverse stored from the previous function, it will 
## calculate the inverse and provide it. 


cacheSolve <- function(x, ...) {
        	invm <- x$getinverse()
        	if(!is.null(invm)) {
               	message("getting cached data")
               	return(invm)
      	  }
       	  data <- x$get()
        	  invm <- solve(data, ...)
        	  x$setinverse(invm)
              invm
}	

