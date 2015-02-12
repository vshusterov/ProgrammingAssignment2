## The makeCacheMatrix function creates a special "matrix", which is really a matrix containing a function to

## set the value of a matrix
## get the value of a matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) 
        {
          x <<- y
          inv <<- NULL
        }
      get <- function() x
      set_inverse <- function(inverse) inv <<- inverse
      get_inverse <- function() inv
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the solve function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
      inverse <- x$get_inverse()
      if(!is.null(inverse)) 
        {
          message("getting cached data.")
          return(inverse)
        }
      data <- x$get()
      inverse <- solve(data)
      x$set_inverse(inverse)
      inverse
}
