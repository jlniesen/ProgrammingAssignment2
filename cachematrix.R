## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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
