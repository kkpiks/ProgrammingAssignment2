## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## This function consists of methods get, set, getInverse, setInverse

makeCacheMatrix <- function(x = matrix()) 
{
  if(makeCacheMatrix(is.null))
  {
    message("input a proper function")
  }
  else
  {
    opposite <- NULL ##inverse set as null
    set <- function(y)
    {
      x <<- y
      opposite <<- NULL  ##inverse set as null again
    }
    get <- function() x
    setInverse <- function(inverse) opposite <<- inverse ##sets the variable opposite with the method inverse 
    getInverse <- function() opposite
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse) ##creates a list
  }
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  opposite <- x$getInverse()
  if(!is.null(opposite)) ## a conditional if opposite is null
  {
    message("processing cached data")
    return(opposite) ##this returns the inverse
  }
  matrice <- x$get()
  opposite <- solve(matrice, ...)
  x$setInverse(opposite)
  opposite 
  message("Success!")
}
