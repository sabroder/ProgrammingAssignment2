# Caching the inverse of a matrix
    # First, cache the inverse of a matrix
    # Second, grab the cached inverse of the matrix, if it exists,
    # otherwise, calculate the inverse
    

makeCacheMatrix <- function(mat = numeric()) {
# Creating a function that forms a matrix object, caching its inverse.  
    m <- NULL
      set <- function(y) {
            mat <<- y
            m <<- NULL
      }
      get <- function() mat
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
# Creating a function that computes the inverse of the matrix
# Grabs the cached inverse if it exists; otherwise, computes the inverse
    m <- x$getInverse()
      if(!is.null(m)) {
            message("Getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
