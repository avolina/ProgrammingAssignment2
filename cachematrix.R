## So the first function defines 4 other functions that get called
## by the second function. The second function starts with checking 
## if the cached value of inversed matrix for input matrix x already
## exists. If it does exist it pulls the value from cache with 
## relevant message. If it doesn't exist it runs the inversion 
## (solve function) and writes it into cache. Then it returns the 
## value of inversed matrix.

## See above.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## See above.

cacheSolve <- function(x, ...) {
  inv2 <- x$getinv()
  if(!is.null(inv2)) {
    message("getting cached data")
    return(inv2)
  }
  data <- x$get()
  inv2 <- solve(data, ...)
  x$setinv(inv2)
  return(inv2)
}

## Thank you for reviewing my code! :)
