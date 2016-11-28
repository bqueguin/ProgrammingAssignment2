# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## This function creates a list containing functions to:
# * set the value of the matrix
# * get the value of the matrix
# * set the value of the inverse matrix
# * get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solv) s <<- solv
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function return the inverse of the matrix x
# cacheSolve: This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if (!is.null(s)) {
        message("getting the matrix in the cache")
        return(s)
    }
    data <- x$get()
    if (det(data)==0) {
        s <- "Matrix can't be inversible (det=0)"
        x$setsolve(s)
        return(s)
    }
    s <- solve(data)
    x$setsolve(s)
    s
}

M <- matrix(c(0,3,0,3,4,2,0,0,1),nrow=3)
cacheM <- makeCacheMatrix(M)
inv <- cacheSolve(cacheM)
print(inv)

