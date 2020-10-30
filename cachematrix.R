## These functions permit calculating the inverse of a given matrix and, 
## once calculated, it is cached

## This function creates a matrix object x capable of storing its inversed
# version

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function calculates the inverse of the matrix from the above function, 
# if the inverse has not yet been calculated or the matrix has changed recently.
# Otherwise, it returns the stored version

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setmatrix(matrix)
  matrix
}


