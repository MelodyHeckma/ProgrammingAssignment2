## below are a pair of functions that are used to create an object 
## that stores a matrix and caches its inverse

## the following function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the following function solves for the inverse of the special "matrix" created by the above function
## however, it first checks to see if the inverse has already been calculated
## if so, it retrieves the inverse from the cashe
## otherwise, it calculates the inverse and sets that value in the setInverse function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

