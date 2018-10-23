## To write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
#### 1. Set a matrix
#### 2. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    SetInver <- function(solveMatrix) inver <<- solveMatrix
    GetInver <- function() inver
    list(set = set, get = get, SetInver = SetInver, GetInver = GetInver)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$GetInver()
    if(!is.null(inver)) {
        message("Getting cached data!")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$SetInver(inver)
    inver
}
