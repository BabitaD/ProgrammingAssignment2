## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() {x}
  set_inverse <- function(solve) {inv <<-solve}
  get_inverse <- function() {inv}
  return(list(get = get, set = set, 
       get_inverse = get_inverse,
       set_inverse = set_inverse))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if (!is.null(inv)){
      message("getting cached data...")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    return(inv)
}
