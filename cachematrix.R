## The following two functions help improving the time or speed taken in calculating an inverse matrix
## by leveraging the caching process

## This function initializes the cached matrix to be inversed

makeCacheMatrix <- function(x = matrix()) {
  prev_matrix <- NULL
  set <- function(y) {
    x <<- y
    prev_matrix <<- NULL
  }
  get <- function() x
  setprevmatrix <- function(x) prev_matrix <<- x
  getprevmatrix <- function() prev_matrix
  list (set = set, get = get, setprevmatrix = setprevmatrix, getprevmatrix = getprevmatrix)
}


## This function will inverse the cached matrix

cacheSolve <- function(x, ...) {
  prevmatrix <- x$getprevmatrix()

  if(!is.null(prevmatrix)){
    if(identical(x$get(),solve(prevmatrix))){
      return(prevmatrix)
    }
  }
  x$setprevmatrix(x)
  m <- solve(x$get())
  return(m)
}
