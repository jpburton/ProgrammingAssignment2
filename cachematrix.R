## Code to compute the inverse of a matrix. Will cache the result when computed,
## and use this cached version as long as the matrix remains the same. 

# Function which describes a cached matrix. Stores the value of that matrix 
# and also stores it's inverse (if calculated). 
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   # sets the value of the matrix to be solved. Since this is a new value,
   # reset the cached inverse to NULL so it's re-calculated.
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

## Solves the inverse of the provided matrix, using a cached value if one is available. 
## The matrix provided is an instance of a matrix created using makeCacheMatrix.
cacheSolve <- function(x, ...) {
  # get the cached version
  i <- x$getinverse()
  # if it's been set, then use this
  if (!is.null(i)) {
    return(i)
  }
  # otherwise retreive the matrix and solve it's inverse
  matrix <- x$get()
  i <- solve(matrix, ...)
  # store the calcuated value 
  x$setinverse(i)
  # return the value computed
  i
}
