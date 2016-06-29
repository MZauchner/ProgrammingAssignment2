## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache
cachesolve <- function(x, ...) {
 ## attempt to get the inverse of the matrix stored in cache

  m <- x$getinverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

