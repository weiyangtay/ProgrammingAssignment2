## These 2 functions take a square matrix as an input, and then provide the inverse or cached inverse of the matrix as the output
## 

## This function (makeCacheMatrix) takes in a square matrix and stores the matrix and its inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function calculates and returns the inverse from the stored square matrix provided in makeCacheMatrix

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

