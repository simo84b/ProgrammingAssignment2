## makeCacheMatrix creates a matrix, which has got methods for
## 1. Getting and setting the matrix (x$get(), x$set(newMatrix))
## 2. Getting and setting the matrix inverse (x$getinverse(), x$setinverse(newInverseMatrix))

makeCacheMatrix <- function(x = matrix()) {

    matrixInv <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        matrixInv <<- NULL
    }
    get <- function() x
    setinverse <- function(newInverse) matrixInv <<- newInverse
    getinverse <- function() matrixInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that returns
## 1. the matrix inverse cached before (if there is one), OR
## 2. the matrix inverse after having calculated and cached it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getinverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setinverse(inverseMatrix)
    inverseMatrix
}
