## Inside this script, there is 2 function to calculate the inv of a 
## matrix fast as possible (using cache)
## Created by Aurelien Briand the 20/02/2015
## Version 1.0


## Create a matrix and cache the inverse of this matrix

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


## Calculate the inverse. If the result is inside the cache, use the cache.
## Or calculate it and put it into the cache

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

        ## Return a matrix that is the inverse of 'x'fin
}
