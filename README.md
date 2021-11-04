# getting a cache matrix in order to find the inverse of a matrix
# we create the matrix using the following formular
# we create the matrix x and assign it to cacheMatrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
    x <<- y
    inv <<- NULL   # this creates a cacheMatrix
}
get <- function() x
setInverse <- function() inv <<- solve (x) 
getInverse <- function() inv
list (set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null (inv))  {
           message ("getting cached data")
           retun (inv)
         }
         mat <- x$get()
         inv <-solve(mat,...)
         x$setInverse(inv)
         inv
 }     # this returns the inverse of the matric x 
