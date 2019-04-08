## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a function that can create the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  inverseMatrix <- NULL
  setMatrix <- function(y)
  {
    x <- y
    inverseMatrix <- NULL
  }
  getMatrix <- function() x   #gets value of matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse ##set the inverse value of the matrix
  getInverse <- function() inverseMatrix ##gets the value of the inverse
  list (setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  }


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <-x$getInverse()
  if(!is.null(inverseMatrix))
  {
    message("getting inverse of cached matrix")
    return (inverseMatrix)
  }
  data <-x$getMatrix()
  inverseMatrix <- solve(data,...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
