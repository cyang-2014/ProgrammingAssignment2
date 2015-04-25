## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    InverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        InverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) InverseMatrix <<- inverse
    getInverse <- function() InverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## NOTE:For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    InverseMatrix <- x$getInverse()
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }
    data <- x$get()
    InverseMatrix <- solve(data)
    x$setInverse(InverseMatrix)
    ## Return a matrix that is the inverse of 'x'
    InverseMatrix
}



## test code example:
# x <- rbind(c(1, -1/2), c(-1/2, 1))
# m <- makeCacheMatrix(x)
# m$get()
# 
# cacheSolve(m)
# cacheSolve(m)