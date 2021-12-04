## Put comments here that give an overall description of what your
## functions do

## An adaptation of the example code. Here i just changed the type of argument from
## numeric to a matrix and changed some variable names.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Same ting here. The only difference is that instead of using mean(x) i used solve(x), which 
## assign the inverse of the matrix to the inv variable.
## The function first assign the result of the getinverse function to the inv variable and check it's value.
## If it return a non NULL value, then the function just returns the value stored in the inv variable.
## if inv is a NULL value, then the function get the matrix and stores it in the data variable, then it calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

