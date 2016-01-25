## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    library(MASS)
    invM <- NULL
    set <- function(y) {
        m <<- y
        invM <<- NULL
    }
    get <- function() m
    setInvM <- function(invM1) invM <<- invM1
    getInvM <- function() invM
    list(set = set, get = get,
         setInvM = setInvM,
         getInvM = getInvM)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInvM()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- ginv(data)
    x$setInvM(m)
    m
}
