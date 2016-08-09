## 1.makeCacheMatrix: This function creates a special "matrix" object that caches its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.


## Write a short comment describing this function
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { ## set the value of the vector
                x <<- y  
                inv <<- NULL
        }
        get <- function() x ## get the value of the vector
        setInverse <- function(inverse) inv <<- inverse ## set the value of the data to be inversed
        getInverse <- function() inv ## get the value of the data to be inversed
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() ## checks to see if the data has already been calculated
        if (!is.null(inv)) {  ##  If so, it gets the data from the cache and skips the computation
                message("getting cached data") ## skipping computation
                return(inv) ## getting the cached data
        }
        mat <- x$get()  ## calculates the the data
        inv <- solve(mat, ...) ## assigns the inverse of a square matrix
        x$setInverse(inv) ## sets the value of the inverse in the cache via the setInverse function
        inv
}