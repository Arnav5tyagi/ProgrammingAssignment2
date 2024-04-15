makeCacheMatrix <- function(x = matrix()) {
  cache <- list(matrix = x, inverse = NULL)

  set <- function(y) {
    cache$matrix <<- y
    cache$inverse <<- NULL
  }

  get <- function() cache$matrix

  setInverse <- function(inverse) {
    cache$inverse <<- inverse
  }

  getInverse <- function() {
    cache$inverse
  }

  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}

cacheSolve <- function(x, ...) {
  if (!is.null(x$getInverse())) {
    message("getting cached data")
    return(x$getInverse())
  }

  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  return(inverse)
}
