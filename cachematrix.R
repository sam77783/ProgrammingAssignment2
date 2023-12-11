## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #set the inverse matrix as NULL
    inv_m <- NULL
    #set fxn to set matrix
    set <- function(y) {
      x <<- y
      inv_m <<- NULL
    }
    
    
    get <- function() x
    setInv <- function(inverse) inv_m <<- inverse
    getInv <- function() inv_m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
## Fxn returns the inverse of matrix. First check whether the inverse exists or not. If yes, get the results and skip computation. If no, compute the inverse and set the value in the cache.
## Assume matrix is always invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getInv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setInv(inv_m)
  inv_m
}