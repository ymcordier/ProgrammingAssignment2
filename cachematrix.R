##
## This function creates a special "matrix" object from a matrix
## This object contains a set of functions:
## - to set or get the value of the matrix
## - to set or get the value of a computation
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get the value of the matrix	
    get <- function() x
    # set the value of the computation result
    setresult <- function(computation) m <<- computation
    # get the value of the computation result
    getresult <- function() m
    
    list(set = set, get = get,
         setresult = setresult,
         getresult = getresult)
}

##
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getresult()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setresult(m)
    m
}