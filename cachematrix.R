## Overall, the pair of functions create the inverse of a matrix.
## As this can be time-consuming, they cache the inverse 

## The first function creates the environment in which several functions are saved
## First, 'm' is created and defined as null
## Second, 'set' is created, and sets the value of the matrix
## Third, 'get' returns the value of the matrix
## Fourth, 'setinv' sets the value of the inverse
## Fifth, 'getinv' returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function looks to the above environment that was created
## If the inverse has already been calculated, it returns that 
## Otherwise it makes the calculation itself

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
        

