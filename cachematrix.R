## Assignment of Week 3 of R-Programming Module from
## Data Science Specialization by Johns Hopkins University
## in Coursera.com

m <- matrix(c(2,2,1,2),nrow = 2,ncol = 2)

## This function contains a list of functions to set and retrieve a matrix and
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set_matrix <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get_matrix <- function() x
  set_inverse_matrix <- function(inverse_matrix) invm <<- inverse_matrix
  get_inverse_matrix <- function() invm
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}


## This function execute a inverse matrix or execute a cache inverse
## matrix if it is cached. This function uses the previous one.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$get_inverse_matrix()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get_matrix()
  invm <- solve(data, ...)
  x$set_inverse_matrix(invm)
  invm
}
