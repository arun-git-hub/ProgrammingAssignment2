
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.

## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

makeCacheMatrix <- function(x = matrix()) {
i <- NULL                                                         ## Initially assigning 'NULL' to inverse
  set <- function(y) {
    x <<- y                                                       ## Setting the matrix 'x'
    i <<- NULL
  }
  get <- function() x                                             ## Returning matrix 'x'
  setinverse <- function(inv) i <<- inv                           ## Cache the value of the inverse 
  getinverse <- function() i                                      ## Returning inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                                  ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()                                          ## Getting inverse
     if(!is.null(i)) {                                            ## Checking for the presence of inverse
        message("getting cached data")                            ## Displaying message
        return(i)
  }
  m <- x$get()                                                    ## Getting Matrix
  i <- solve(m, ...)                                              ## Using solve() to compute inverse
  x$setinverse(i)                                                 ## To cache the inverse
  i                                                               
}


