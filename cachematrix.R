## The first function, makeCacheMarix creates a cached matrix.
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
    
}


## The second function, cacheSolve inverses the matrix from 'makeCacheMatrix' unless the matrix has already been inversed.

cacheSolve <- function(x, ...) {

        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Someone already did the work, lucky us!")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse

}
