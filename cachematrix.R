## This function takes a matrix, computes the inverse, and stores it in environment
## if the matrix has not changed on furture calls, it gets the stored value from
## the environment instead of computing the inverse again

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates the constructor fuction used to store or solve the given matrix
  
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


## Write a short comment describing this function
## This function uses the constructor functionality of makeCacheMatrix 
## to determine if the inverse of the matrix should be recalled from the environment
## or calculated 

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