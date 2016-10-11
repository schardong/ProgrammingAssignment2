##' Function to add caching support for the base matrix type.
##'
##' This function adds caching support for the inverse of a basic R matrix type.
##' By adding caching support, the inverse of a matrix will be calculated only
##' once, any subsequent calls will return the results calculated previously.
##' 
##' @title Cache building function for the matrix inverse operation.
##' @param x The matrix to be inverted. Default value is an empty matrix.
##' @return A list of methods to enable the caching of the matrix inverse
##' operation.
##' @author Guilherme G. Schardong
##' @seealso \core{cacheInverse}
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

##' Function to solve the matrix inverse problem with caching support.
##'
##' This function calculates the inverse of a matrix. The input matrix is built
##' by using the \code{makeCacheMatrix} function in order to add caching support
##' to the base matrix. In this way, future calls to this function using the same
##' matrix will return the already calculated inverse makeing the process much
##' faster.
##' 
##' @title Caching solution for the matrix inverse operation.
##' @param x The matrix to be inverted as returned by the \code{makeCacheMatrix}
##' function.
##' @param ... Other parameters for the \code{solve} function.
##' @return Returns the matrix inverse of \code{x}.
##' @author Guilherme G. Schardong
##' @seealso \code{solve}, \code{makeCacheMatrix}.
##' @examples
##' A <- matrix(data = rnorm(1000000), nrow = 1000)
##' mat.cache <- makeCacheMatrix(A)
##' for (m in 1:1000) {
##'     mat.inv <- cacheSolve(mat.cache)
##' }
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
