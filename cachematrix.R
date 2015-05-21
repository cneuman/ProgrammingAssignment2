## This function takes a sqaure matrix and then takes the inverse and caches it so that it is retrievable later.

## The makeCacheMatrix function takes the sqaure matrix and computes the inverse

# Input to the function is a matrix
# Easy input example is "input_mat = matrix(runif(5*5), ncol=5)"
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function essentially just retrieves the solved sqaure matrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}