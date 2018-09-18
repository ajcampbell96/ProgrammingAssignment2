## These functions work together to cache the inverse of a matrix. makeCacheMatrix will create a special matrix
## with getters and setters to get/set the data and inverse of the matrix. cacheSolve will either grab the inverse
## of the matrix (if already done and cached) or it will calculate the inverse and cache it so it doesn't have to be
## calculated later on.

## makeCacheMatrix creates a special "matrix" which is a list containing a function to set the matrix,
## get the matrix, set the inverse of the matrix, or get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()){
  m <- NULL # Set m to initially be null
  set <- function(y){ # This is a setter function, sets the values passed to it
    x <<- y
    m <<- NULL
  }
  get <- function() x # This is a getter function, gets the matrix
  setinverse <- function(inverse) m <<- inverse # This sets the inverse to m
  getinverse <- function() m # This gets the inverse, contained in m
  # Create list with the set of setters and getters
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix created by makeCacheMatrix. First, it checks to
## make sure the inverse hasn't already been done... if so it just grabs that. If not, it uses the 'solve'
## function to create the inverse and caches it with makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() # get the inverse... if it's there it's a matrix, if not it's NULL
  if(!is.null(m)){ # If m actually exists, then just return m
    message("getting cached data")
    return(m)
  }
  data <- x$get() # Get the data
  m <- solve(data, ...) # this inverse the matrix
  x$setinverse(m) # This sets the inverse cache
  m
}
