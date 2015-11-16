## Solving and Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly.
## Functions below can be used to create a special object that 
## stores a matrix and caches its inverse from the Environment.

## Function No. 1
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  invurs <- NULL
  
  set <- function(y) 
  {
    x <<- y
    invurs <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) invurs <<- solve
  
  getInverse <- function() invurs
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## End of the function script.

## Function No. 2
## This function computes the inverse of the "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## Otherwise, it solves the matrix and reports the inverse.

cacheSolve <- function(x, ...) 
{
  invurs <- x$getInverse()
  
  if (!is.null(invurs)) 
  {
    message("Getting the data from the cache")
    return(invurs)
  }
  
  matrx <- x$get()
  invurs <- solve(matrx, ...)
  x$setInverse(invurs)
  invurs
}

## End of the function script.
## End of the file. 
