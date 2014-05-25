## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL						##the inv cache
        set <- function(y) {		##set matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x			##get matrix
        setinv <- function(inversion) inv <<- inversion		##set inv cache
        getinv <- function() inv							##get inv cache
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()				##get inv from the cache
        if(!is.null(inv)) {			##if inv from the cache is not null, return it 
                message("getting cached data")
                return(inv)
        }
        data <- x$get()				##if inv from the cache is null ,calculate inv using solve() function
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
