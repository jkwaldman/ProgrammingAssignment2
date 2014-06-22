# cacheMatrix creates a cacheable matrix object given a matrix
# cacheSolve takes that matrix, checks if it has been computed, and
# returns the inverted matrix.

# Creates a cacheable matrix object given a matrix

makeCacheMatrix <- function(x = matrix()) {
    cacheVal <- NULL
    setMatrix <- function(y) {
        x <<- y
        cacheVal <<- NULL
    }
    getMatrix <- function() x
    setCachedMatrix <- function(matrix) cacheVal <<- matrix
    getCachedMatrix <- function() cacheVal
    # Creates the matrix object
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setCachedMatrix = setCachedMatrix, getCachedMatrix = 
         getCachedMatrix)
}


# Takes the result of makeCacheMatrix() and find the inversion
# of that matrix object

cacheSolve <- function(x, ...) {
    cacheVal <- x$getCachedMatrix()
    #Only true if the value has been computed before
    if(!is.null(cacheVal)){
        message("Getting cached data..")
        return(cacheVal)
    }
    #Hasn't been computed, so find the matrix inversion
    matrixVal <- x$getMatrix()
    solvedMatrix <- solve(matrixVal)
    x$setCachedMatrix(solvedMatrix)
    solvedMatrix
    
# Returns a matrix that is the inverse of the given matrix
}
