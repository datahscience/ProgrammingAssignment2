## The following pair of functions allows the caching
## of the inverse of a matrix

## This function take a matrix and returns a special object
## that caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverm <- function(inverm) inverseMatrix <<- inverm
  getInverm <- function() inverseMatrix
  list(set = set, get = get,
       setInverm = setInverm,
       getInverm = getInverm)
}


## This function returns the cached inverse of a mtarix

cacheSolve <- function(x, ...) {
  m <- x$getInverm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverm(m)
  m
}
