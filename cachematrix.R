## The first function below calculates and saves the inverse of a matrix to cache 
## The second function, retrieves the inverse from cache if it has not changed. 

## makeCaheMatrix contains four functions that retrieve a matrix, save a matrix to cache, 
## calculate and save the inverse of a matrix to cache and and retrieve the inverse.

makeCacheMatrix <- function(m = matrix()) {
        s <- NULL
        set <- function(inMatrix) {
                m <<- inMatrix
                s <<- NULL
        }
        get <- function() {
                m
        }
        
        setSolve <- function(inSolve) { 
                s <<- solve(inSolve)
        }
        
        getSolve <- function() {
                s
        }
        
        list(set = set, 
             get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve takes in a matrix and returns its inverse. Compares 
## the inverse to the inverse in cache. If different
## recalculates the inverse otherwise pulls it from cache.

cacheSolve <- function(x, ...) {
        
        
                s <- x$getSolve()
                
                if(!is.null(s) && s == solve(x$get()) ) {
                        message("getting cached data")
                        return(s)
                }
                
                data <- x$get()
                
                s <- solve(data, ...)
                
                x$setSolve(s)
                
                s 
}
