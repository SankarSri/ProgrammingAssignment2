## These set of functions make inverse matrix operation efficient by caching the results. These functions are
## most useful for computing inverse of large matrix in case there is a need for repetitive need for inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse, thus saving compute time for large and repetitive
## calls to compute inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setCacheMatrix <- function(y)
  {
    x <<- y
    invMatrix <<- NULL
  }
  getCacheMatrix <- function() x
  setInvCacheMatrix <- function(toInvM) invMatrix <<- toInvM
  getInvCacheMatrix <- function() invMatrix
  list(setCacheMatrix = setCacheMatrix,
       getCacheMatrix = getCacheMatrix,
       setInvCacheMatrix = setInvCacheMatrix,
       getInvCacheMatrix = getInvCacheMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInvCacheMatrix()
  if (!is.null(invMat))
  {
    message("Getting cached data")
    return(invMat)
  }
  origMat <- x$getCacheMatrix()
  invMat <- solve(origMat,...)
  x$setInvCacheMatrix(invMat)
  invMat
}
