## The following two functions help improving the time or speed taken in calculating an inverse matrix
## by leveraging the caching process

## This function initializes the setter and getter of the matrix to be inversed, the storage of the "previous" matrix and also the inversed input matrix

makeCacheMatrix <- function(x = matrix()) {

  ##initialize variables will be used to store the caches
  prev_solve <- NULL
  prev_matrix <- NULL
  
  ##initialize the setter and getter for the input matrix
  set <- function(y) {
    x <<- y
    prev_solve <<- NULL
	prev_matrix <- NULL
  }
  get <- function() x
  
  ##initialize the setter and getter for the inverse of the input matrix
  setsolve <- function(x) prev_solve <<- x
  getsolve <- function() prev_solve
  
  ##initialize the setter and getter for the PREVIOUS input matrix
  setprevmatrix <- function (x) prev_matrix <<- x
  getprevmatrix <- function() prev_matrix
  
  ##create a list of function to set and get the matrix needed 
  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve, 
        setprevmatrix = setprevmatrix,getprevmatrix = getprevmatrix)
}


## This function will calculate the inverse of the input matrix or retrieving the cached inverse matrix if the input matrix is the same

cacheSolve <- function(x, ...) {

  ##store the variables which work as a chcecker of the previous input matrix and its inversed result
  prevsolve <- x$getsolve()
  prevmatrix <- x$getprevmatrix()

  ##If this is not the first time that the "cacheSolve" function is run, then check if the current input matrix is IDENTICAL with the previous one
  ##If they are IDENTICAL, then retrieve the inverse of the curent input matrix from PREVSOLVE instead of recalculating the inverse
  if(!is.null(prevmatrix)){
    if(identical(x$get(),prevmatrix)){
      message("getting cached data")
      return(prevsolve)
    }
  }
  
  ##if this is the first time that the "cacheSolve" function is run OR the current input matrix is not IDENTICAL with the previous one,
  ##then replace the PREVMATRIX and PREVSOLVE with the current input matrix and its inverse respectively
  x$setprevmatrix(x$get())
  m <- solve(x$get())
  x$setsolve(m)
  return(m)
}
