##The first function, makeCacheMatrix creates a special "matrix",
##which is really a list containing a function to set the 
##elements of the matrix, get the elements of the matrix, set the elements 
##of the inverse, and get the elements  of the inverse
## the input matrix is assumed to be invertible and therefore, square.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if 
##the inverse has already been calculated. If so, it gets the 
##inverse from the cache and skips the computation. Otherwise, 
##it calculates the inverse of the matrix and sets the value of the 
##inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
##some invertible matrices to try out the functions:
##A = matrix( c(3, 0, 2, 2, 0, -2, 0, 1, 1),nrow=3, ncol=3, byrow = TRUE)
##B = matrix( c(-7, -6, -12, 5, 5, 7, 1, 0, 4),nrow=3, ncol=3, byrow = TRUE)
