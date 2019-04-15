## Course 2 Week/Assignment 3: Caching the Inverse of a Matrix
## Showing a pair of functions (makeCacheMatrix and cacheSolve) that create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() I
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix. 
## Note:  If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        I <- x$getinverse()
        if (!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        mat <- x$get()
        I <- solve(mat, ...)
        x$setinverse(I)
        I
}


###Testing functions###

M_matrix <- makeCacheMatrix(matrix(1:4,2,2))
M_matrix$get()

M_matrix$getinverse()

cacheSolve(M_matrix)

