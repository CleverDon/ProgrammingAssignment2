## Put comments here that give an overall description of what your
## functions do
## These functions will solve for the inverse of a matrix.  
##   It also will cache the matrix results and return the cache version
#    if it is called again

## This function will store a matrix an the inverse matrix results.  It has 4 functions
##   that you can call to do this get, set, setmatrix, and getmatrix

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }

## This function will solve for the inverse of a matrix.  It will also cache and return
##     a cached inverse if it has been calcualated already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
  }

