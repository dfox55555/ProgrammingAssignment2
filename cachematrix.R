## 

## This function creates a matrix object

makeCacheMatrix <- function(x = matrix()) {  ## Define the argument for the function
        m <- NULL                            ## Initialize M to NULL
        set <- function(y) {                 ## Define the Set function
                x <<- y
                m <<- NULL
        }
        get <- function() x                  ## Define the Get function
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function computes the inverse of the matrix
## If the inverse was already calculated and has not changed then use the cache values

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)            ## Set the inverse 
        x$setsolve(m)
        m
}