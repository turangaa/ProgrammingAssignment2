##The following two functions, can save us time in calculating the inverse of a matrix, if the inverse is cached and the matrix has not changed. For convenience, we assume the input is always a square invertible matrix, so no check is being performed 


## The makeCacheMatrix creates a special "matrix" object that can cache its inverse. The function is then used in the second function to convert the input to the right "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
  
}


## This function calculates the inverse of the given matrix, after converting it to the special "matrix" object created with the abov function. The function first checks if inverse has been caclulated and skips computation. Otherwise, calculates inverse with solve() function and sets the value in the cache via the setInv function.

cacheSolve <- function(x, ...) {
  m <- makeCacheMatrix(x)$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- makeCacheMatrix(x)$get()
  m <- solve(data, ...)
  makeCacheMatrix(x)$setInv(m)
  m
  
}



