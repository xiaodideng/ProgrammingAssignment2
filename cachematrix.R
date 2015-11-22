## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates special matrix object, and calcuate the inverse.
## If the inverse matrix is calculated, it will instead find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve returns the inverse of a matrix A created by
## makeCacheMatrix function.
## If the inverse is already available, cacheSolve only retrieves, 
## If not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}
}
