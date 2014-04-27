#Functions to calculate the inverse of a matrix or get it from the cache

#********************************************************************************************************************
#Creates a special "matrix" object that seeks for its inverse in cache. 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

#*********************************************************************************************************************
#Checks if the inverse has been already calculated, if so, gets the inverse from the cache and skips computation.
#If not, calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
      
cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}      
