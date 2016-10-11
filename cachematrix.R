##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Cache building function for the matrix inverse operation.
##' @param x The matrix to be inverted. Default value is an empty matrix.
##' @return A list of methods to enable the caching of the matrix inverse
##' operation.
##' @author Guilherme G. Schardong
##' @examples
##' A <- matrix(data = rnorm(1000000), nrow = 1000)
##' mat.cache <- makeCacheMatrix(A)
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Caching solution for the matrix inverse operation.
##' @param x The matrix to be inverted.
##' @param ... Other parameters for the \code{solve} function.
##' @return Returns the matrix inverse of \code{x}.
##' @author Guilherme G. Schardong
##' @seealso \code{solve}
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("Matrix inverse: Using cached data.")
        return(inverse)
    }

    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
