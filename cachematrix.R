## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
## var <- Matrix
## vevt1 <- makeCacheMatrix(var)
## cachesolve(vect1)
## makeCacheMatrix is a function that creates a special vector of
## functions that store and retreive a matrix and invert and store
## solution to the inversion of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    message('set')
    x <<- y
    m <<- NULL
  }
  get <- function() {
    message('got') 
    x
  }
  setmxsol <- function(solve.it) {
    message("setmxsol")
    m <<- solve.it
  }
  getmxsol <- function() 
    {
    message('getmxsol')
    m
    }
  list(set = set, get = get,
       setmxsol = setmxsol,
       getmxsol = getmxsol)
  }



## Returns a matrix that is inverse of x by solving for 'x'
## or retreiving it from storage if it was previously calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmxsol()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmxsol(m)
  m
  
}

