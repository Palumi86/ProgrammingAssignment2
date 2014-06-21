## The overall goal of these functions is to cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object that will be
    ## able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve){
    m <<- solve
  } 
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned
#    by makeCacheMatrix above. If the inverse has already been calculated (and 
#    the matrix has not changed), then cacheSolve should retrieve the inverse from
#    the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  return(m)
}
