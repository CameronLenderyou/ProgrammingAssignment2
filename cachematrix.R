## The inverse of a invertible matrix is cached by the makeCacheMatrix function 
## and is retrieved by the cacheSolve function as long as the inverse has already
## been calculated.

## The first function creates a list containing functions to set the value of 
## the matrix, get the value of the matrix, set the value of the inverse and get
## the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The second function deciphers the inverse of the invertible matrix returned 
## by the makeCacheMatrix function. With the inverse being already calculated by 
## the first function, the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
