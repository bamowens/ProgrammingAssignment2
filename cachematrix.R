## There are two functions:
## makeCacheMatrix - creates a special "matrix" object that can cache its inverse.
## cacheSolve - computes the inverse of the special "matrix" 
##            returned by makeCacheMatrix above. If the inverse has already been calculated 
##            (and the matrix has not changed), then the cachesolve should retrieve the 
##            inverse from the cache.

## create a special "matrix" object 
##    (that is really a list of funtion to set, get, setinv, and get inv)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the matrix value and null out the inverse solution
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse as the value input
  setinv <- function(inv) m <<- inv
  # get and output the inverse
  getinv <- function() m
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Compute the inverse of the special "matrix" returned by makeCasheMatrix,
##    unless the inverse has already been calculated, then cacheSolve retrieves it

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  # check if inverse is cached yet and output if so
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # solve for inverse and output
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}


