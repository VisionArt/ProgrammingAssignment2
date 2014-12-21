## These functions take a matrix, compute and caches its inverse.
## Assumption: The supplied matrix is always invertible.

## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  set <- function(y) {
    x <<- y
    cachedinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedinverse <<- inverse
  getinverse <- function() cachedinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the function should retrieve the inverse from the cache and skip the computation.
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  cachedinverse <- x$getinverse()
  if(!is.null(cachedinverse)) {
    message("getting cached data...")
    return(cachedinverse)
  }
  data <- x$get()
  cachedinverse <- solve(data, ...)
  x$setinverse(cachedinverse)
  cachedinverse
}
