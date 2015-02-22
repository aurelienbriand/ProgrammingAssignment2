## Inside this script, there are 2 functions to calculate the inverse of a 
## matrix fast as possible (using cache)
## Created by Aurelien Briand the 20/02/2015
## Version 1.0


## This function create a matrix and can cache the inverse of this matrix.
## Inside this function you can find a set/get function and a setinv-getinv for the inverse.
## The setinv function is used to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }

        get <- function() x

        setinv <- function(solve)
	  {
		inv <<- solve
	  } 

        getinv <- function() inv

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Calculate the inverse. If the result is inside the cache (!is.null(inv)) , use the cache.
## Or calculate it and put it into the cache in order use next time.
## This function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

       inv <- x$getinv()
       if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        return(inv)
}
