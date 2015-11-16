## Solving and Caching the Inverse of a Matrix:
## Matrix inversion is usually a RAM/resource intensive computation for a computer.  
## There may be some (??) benefit to caching the inverse of a matrix rather than computing it repeatedly.
## Functions below can be used to create a special object that 
## stores a matrix and caches its inverse from the Environment.

## NOTE:
## The instructor's script was removed and was replaced with the Kay's Script.

## Function No. 1
## This function creates a special "matrix" object that can cache its inverse.

## The first line of the script below introduces a function which can take a whole matrix 
## as its argument. Wow!

## Function No. 1
makeCacheMatrix <- function(x = matrix()) 
{
## Initializes the inverse of the function as a NULL matrix invurs. So simple yet elegent!!!  
  invurs <- NULL
## Sets the 'set' as the argument and 'inverse' as empty or NULL matrix  
  set <- function(y) 
  {
    x <<- y
    invurs <<- NULL
  }
## This line simply gives the matrix back through a simple function. No argument to the function are needed.  
  get <- function() x

## Here comes the solution of the problem. Inverse of an invertable matrix is being calculated
## using the solve(x) function in R and stores the results into a variable. 
  
  setInverse <- function(solve) invurs <<- solve
  
  getInverse <- function() invurs
  
  ## This is the most important and powerful part of the function, where
  ## all four components of the function are combined to a list in order
  ## to be subset'ted separately using $ as a separator.
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## End of the Function No. 1 script.

## Function No. 2
## This function computes the inverse of the "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## Otherwise, it solves the matrix and reports the inverse.

cacheSolve <- function(x, ...) 
{
  invurs <- x$getInverse()
## Check here to see if the inverse of the supplied matrix is empty or NULL.
## If Not Empty or NULL, data from memory cache is supplied.
  if (!is.null(invurs)) 
  {
    message("Getting the data from the cache")
    return(invurs)
  }
  
## If the inverse matrix of the given matrix is NULL, it is calculated and solution is supplied
## in setInverse component of the function.
  matrx <- x$get()
  invurs <- solve(matrx, ...)
  x$setInverse(invurs)
  invurs
}

## End of the Function No. 2 script.
## End of the file. 
## This work was not possible without help from DanieleP / PA2-clarifying_instructions.
## Original functions and scripts were from RDPeng of Coursera.
## No copyrights infringement is intended.
## Comments added by Kay Ess on 16 Nov 2016.
