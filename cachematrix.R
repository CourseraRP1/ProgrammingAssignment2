## These functions are defined to cache potentially time-consuming computations on the inverse of a matrix
## to speed op computations

## This function generates a list, that contains functions to:
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the inverse of the matrix
#4.get the inverse of the matrix
## This function can be used to cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
      x <<- y
      mat <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) mat <<- solve
    getinverse <- function() mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## This function can be used to find the inverse of the matrix that is defined through the function CacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inversefrom the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the mean in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  solve <- x$getinverse()
  if(!is.null(solve)) {
    message("getting cached data")
    return(solve)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
