## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
