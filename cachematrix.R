## makeCacheMatrix returns a list of 4 functions, taking a matrix as input
## cacheSolve calculates or returns inverse of matrix depending on the input

## Takes an input matrix and returns a list of 4 functions that set, get values
## of matrix and its inverse as well

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  list (
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
        )
}


## Calculates or returns cached inverse depending on the input object x

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i))
  {
    message("getting cached inverse")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  return(i)
        ## Return a matrix that is the inverse of 'x'
}
