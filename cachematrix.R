## makeCacheMatrix creates a matrix object that can cache its inverse.
## cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix.
## If the inverse was already calculated and the matrix is not changed, then the inverse is retrieved from the cache.
## The function assumes taht the supplied matrix is always invertible.

## makeCacheMatrix has a list of four functions
## 1. set - set the value of the matrix.
## 2. get - get the value of the matrix.
## 3. setmatrix - sets the inverse of the matrix.
## 4. getmatrix - get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL                           
        set <- function(y) {                
                x <<- y
                m <<- NULL
        }
        get <- function() x                 
        setmatrix <- function(solve) m <<- solve 
        getmatrix <- function() m                
        list(set = set, get = get,          
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve returns the inverse of the matrix created with the makeCacheMatrix function.
## The function first checks if the inverse has already been calculated.
## If the inverse has already been calculated, and has not changed, then it returns the value from cache.
## If not present in cache, it calculates the inverse using the setmatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) { #If  m is not null, get the cached data instead
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
