##
## This function creates a special "matrix" object from a matrix
## This object contains a set of functions:
## - to set or get the value of the matrix
## - to set or get the value of a its inverse
##
makeCacheMatrix <- function(x = matrix()) {

        # Initialize the inverse
        i <- NULL
        
        # method to set the matrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        
        # method to get the matrix	
        get <- function() m
        
        # method to set the inverse
        setresult <- function(inverse) i <<- inverse
        
        # method to get the inverse
        getresult <- function() i
        
        # return the list of the methods of the special object
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
        
        # First try to get the result directly from the object 'x'
        m <- x$getresult()
        
        if(!is.null(m)) {
                ## Result is found to be cached in 'x'.
                message("getting cached data")
                ## Return the result and exit the function
                return(m)
        }
        
        # Result was NOT found in cache of 'x' and should be computed now
        
                # 1. get the content of 'x' matrix
                data <- x$get()
                
                # 2. compute the inverse
                m <- solve(data, ...)
                
                # 3. store the result into cache of 'x'
                x$setresult(m)
        
        # Display the result just computed
        m
}