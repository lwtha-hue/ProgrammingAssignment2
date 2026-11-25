## makeCacheMatrix and cacheSolve save time on repeated matrix inversions by storing a matrix alongside its inverse so R only computes the inverse once

## Creates a special "matrix" object that can store a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     # Stores the cached inverse matrix
  set <- function(y) {
    x <<- y       # Updates the matrix in the parent environment
    inv <<- NULL  # Clears any stale cached inverse
  }
  get <- function() x   # Returns the stored matrix
  setinverse <- function(inverse) inv <<- inverse  # Saves the computed inverse to cache
  getinverse <- function() inv  # Retrieves the cached inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Computes the inverse of the special "matrix" object, or returns it from cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()   # Check if the inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)     # Return cached inverse without recomputing
  }
  data <- x$get()    # Get the underlying matrix
  inv <- solve(data, ...)    # Compute the matrix inverse
  x$setinverse(inv)   # Cache the newly computed inverse
  inv
        ## Return a matrix that is the inverse of 'x'
}
