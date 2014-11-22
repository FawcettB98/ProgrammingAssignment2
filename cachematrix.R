## These two functions work together to calculate and cache the inverse of a 
## matrix.  

## First, assign the function makeCacheMatrix to an object.  The object
## will be populated with a list of functions that will be used in the fuction 
## cacheSolve.  

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL     ## sets inverse to NULL when the function is called.  
                        ## This will let cacheSolve know that the inverse has 
                        ## not been calculated yet
    
    set <- function(y) {    ## Allows new values to be assigned to the object
        x <<- y             ## without needing to call the function
        inverse <<- NULL
    }
    
    get <- function() x     ## Allows cacheSolve to access the matrix
    
    setsolve <- function(solve) inverse <<- solve  ## Allows cacheSolve to 
                                                   ## calculate the inverse
    
    getsolve <- function() inverse     # Allows cacheSolve to access the cached 
                                       # inverse
    
    list(set = set, get = get,  # Sets up the list that will be stored in the 
         setsolve = setsolve,     # object
         getsolve = getsolve)
}


## cacheSolve will be called entering the name of the object 
## created before inside the parantheses.  cacheSolve will either calculate the 
## inverse, or pull it from the cached location if it has already been solved.

cacheSolve <- function(x, ...) {
    inverse <- x$getsolve()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setsolve(inverse)
    inverse
}
