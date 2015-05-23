## makeCache creates a list of functions which store / retrieve a matrix 
## and store / retrieve the inverse of a matrix. cacheSolve computes and 
## caches the matrix inverse or retrieves it if it has already been cached

## Creates a special "matrix" object that can cache its inverse
## m is the matrix inverse if cached, x is the matrix inputted to function. 

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function () x                              # retrieves the matrix
  setInverse <- function(inverse) m <<- inverse     # caches the inverse
  getInverse <- function() m                        #retrieves cached inverse
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix
## unless this has already been cached, then call the cached matrix. 

cacheSolve <- function(x, ...) 
{
  ## If inverse is cached return
  m <- x$getInverse() 
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  ## otherwise compute the inverse, cache it and return it. 
  data <- x$get()     
  m <- solve(data,...)     
  x$setInverse(m)     
  m
}
