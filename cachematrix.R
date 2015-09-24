##The two function below cache a matrix in a temporary location and solve the inverse
##for the matrix

##makeCacheMatrix cache a new matrix every time the function is called.  The function
##creates a matrix and allow other function to get this matrix and its inverse.  
##The function also sets a list of what the sub-function can do.

makeCacheMatrix <- function(M = matrix()) {
	I <- NULL
    	set <- function(y) {
        	M <<- y
	    	I <<- NULL
	      }
	      get <- function() M
		setinverse <- function(solve) I <<- solve
		getinverse <- function() I
	list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
		      }


##CacheSolve first check whether the inverse is null.  This function will get
##the matrix from makeCacheMatrix and solve for the inverse and return
##the result to the user.

cacheSolve <- function(M = matrix(), ...) {
  I <- M$getinverse()
    if(!is.null(I)) {
    message("getting cached data")
    return(I)
    }
  data <- M$get()
  I <- solve(data, ...)
  M$setinverse(I)
  I
		       ## Return a matrix that is the inverse of 'x'
}
