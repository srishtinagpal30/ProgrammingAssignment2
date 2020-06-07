## Caching the inverse of matrix 

## function makeCacheMatrix first sets the value and gets the value of the matrix then it sets the value and gets the value of the inverse matrix  

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(f) {
                x <<- f
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) v <<- inverse
        getinverse <- function() v 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function cacheSolve computes the inverse of the matrix by taking the input value from the above function. If value is null then it gets the original matrix and solves for the inverse. 

cacheSolve <- function(x, ...) {
	v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data inverse matrix")
                return(v)
        }
        matrixdata <- x$get()
        v <- solve(matrixdata, ...)
        x$setinverse(v)
        v
}
     
