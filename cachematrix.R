## Compute inverse of a matrix with retrieve
## from cache memory feature

## creates a matrix object that can cache its 
## inverse

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


## computes inverse of a new matrix and retrieves
## from cache if already computed

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
