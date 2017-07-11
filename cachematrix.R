## Programming Assignment 2 - Calculate Matrix Inverse.
## the purpose of the functions is to reduce the calculcation of the inverse matrix calculcation
# in instances where the matrix has not changed.
# The function will cache the matrix calculcation and check on the next call is the matrix value has changes or not.
# If there is no change then simply retrieve the cached value of the inverse calculcation.
# else solve the inverse of the matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Write a short comment describing this function
# Solve matrix m inverse - check first if inverse was already calculated else will produce results
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
