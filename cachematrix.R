## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## This function consists of methods get, set, getInverse, setInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##inverse set as null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##creates a list
}


## Write a short comment describing this function
## This function is used to get the cached data of an matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("processing cached data")
          return(inv) ##this returns the inverse
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv 
}
