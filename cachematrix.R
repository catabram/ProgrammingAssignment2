## Caching inversed matrix for faster computations and memory resources saving 

## Creating matrix which will cache inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  invertion <- NULL
  
  set <- function(y) {
    x <<- y
    invertion <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) invertion <<- inverse
  
  getInverse <- function() invertion
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computation of inversed matrix made by makecachematrix function.
## In case if invertion already computed for certain matrix it will give inversion from cache.


cacheSolve <- function(x, ...) {
  invertion <- x$getInverse()

    if (!is.null(invertion)) {
    message("from cache")
    return(invertion)
  }
  
  matr <- x$get()
  invertion <- solve(matr, ...)
  x$setInverse(invertion)
  invertion
}
