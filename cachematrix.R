## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function has 4 functions itself. get, set, set_inverse, get_inverse
# make matrix and add functions
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y 
    inv_matrix <<- NULL 
  }
  get <- function() x 
  set_inverse <- function(inv) inv_matrix <<- inv 
  get_inverse <- function() inv_matrix
  list(set = set, 
       get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## Write a short comment describing this function
## if matrix values in cache, use it.
## if matrix values is not in cache, calcurate inverse matrix by solve()
## and store inverse matrix in cache for next time.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$set_inverse(inv_matrix)
  inv_matrix
}
