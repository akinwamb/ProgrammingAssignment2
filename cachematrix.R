## The pair of functions below cache the inverse of a matrix
## rather than computing the inverse repeatedly

## This function "makeCacheMatrix" creates a special "matrix" object that
## can cache its inverse. 
## First, the function set and get the value of the matrix
## Second, the function set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) { # The function set the value of the matrix
          x <<- y
        inv <<- NULL
      }
    get <- function() x	 # The function get the value of the matrix
    setinv <- function(solve) inv <<- solve # This will fails if the matrix is noninvertible
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## "makeCacheMatrix" function.

## If the inverse has already been calculated (and the matrix has not changed),
## then the "cacheSolve" function should retrieve the inverse from the
## "makeCacheMatrix" function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
      inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
