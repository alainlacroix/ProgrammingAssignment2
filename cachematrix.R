  makeCacheMatrix <- function(x = numeric()) {
  # This function takes a square invertible matrix as input parameter and returns an object of type list 
  # containing 4 functions: set(), get(), setsolve(), and getsolve(). 
  # The input matrix is stored as a kind of persistent object
  
  
  m <- NULL # initialiaze the inverted matrix object "m"
  
  # The function set() stores the matrix passed as imput parameter. 
  # The matrix is stored in the kind of persistent object "x"
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # The function get() returns the square invertible matrix stored in the kind of persistent object "x"
  get <- function() x
  
  # The function setsolve() stores the inversed matrix passed as parameter (i.e. solve) 
  # The inverted matrix is stored in the kind of persistent object "m"
  setsolve <- function(solve) m <<- solve
  
  # The function getsolve() returns the inverted matrix
  getsolve <- function() m
  
  # A list containing the 4 functions is returned
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  # This function takes a variable returned by makeCacheMatrix() to get the matrix, invert it and store it. 
  # The inverted matrix is returned as output
  
  # If the inverted matrix is already existing return it
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise, 
  data <- x$get()       # get the matrix
  m <- solve(data, ...) # invert it
  x$setsolve(m)         # store it
  m                     # and return it
}
