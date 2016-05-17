## Together these two functions solve a matrix. If the matrix was solved
## previously it will get the solutions from storage

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

## Flow of Use:
## var <- aMatrix
## vevt1 <- makeCacheMatrix(var)
## cachesolve(vect1)



makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix is a function that creates a special vector of
        ## functions that store and retreive a matrix and invert and store
        ## solution to the inversion of a matrix
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmxsol <- function(solve.it) {
                m <<- solve.it
        }
        getmxsol <- function() m
        
        list(set = set, get = get,
             setmxsol = setmxsol,
             getmxsol = getmxsol)
}



cacheSolve <- function(x, ...) {
        ## Returns a matrix that is inverse of 't' when the input vector
        ## created by makeCacheMatrix(t) by solving for inverse of t
        ## or by retreiving it from storage if it was previously calculated
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

