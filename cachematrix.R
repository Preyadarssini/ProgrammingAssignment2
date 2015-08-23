## This program in intended to compute the matrix inversion 
## and cache the inverted matrix

## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

	  ## computes inverse of the matrix
        setMatrix <- function(solve) m <<- solve

	  ## Cache the inverted matrix
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix ,
             getMatrix = getMatrix)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix If the inverse has already been calculated 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
	m <- x$getMatrix()

	## Check for existense of cached matrix 
        if(!is.null(m)) {
                message("getting cached Matrix data")
			## return the cached matrix
                return(m)
        }
	## create a inverse matrix. This step is executed 
	## only if the cached data is not existing
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setMatrix(m)
        m      
}
