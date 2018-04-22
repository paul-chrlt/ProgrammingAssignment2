## 2 functions to solve the inverse of a matrix and cache the result.

## creates a vector containing 4 functions : set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()
    x
  setinverse <- function(solve)
    inv <<- solve
  getinverse <- function()
    inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## returns the inverse of a matrix. Will only compute it if the result is not already stored.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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