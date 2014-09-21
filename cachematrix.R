## The functions take in a matrix and try to find its inverse. If the input matrix is new, the functions will compute its inverse and store it in cache; if the input matrix is not changed, the functions will directly read the cache and skip the computation.

## Function makeCacheMatrix() takes in a matrix and output a list of 4 sub-functions along with the environment where the data is cached in.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL	## Clear old cache every time a new data is read in, also indicates if the data has been changed.
  set <- function(y) {	## Optional, the complete function will do the same thing, but this sub-function can be called separately thus may speed up a bit.
    x <<- y
    cache <<- NULL
  }
  get <- function() x	## Get the value of x whose value was defined by the complete function or the get() sub-function.
  setinverse <- function(inverse) cache <<- inverse	## Assign varible "cache" to a value, under the shared environment of other sub-functions, so that it can be called by getinverse().
  getinverse <- function() cache	## Get the cached value of the inverse of the matrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve() takes in the list returned by makeCacheMatrix(). It first checks if there is already a cached value, if so it uses it directly; if not, it computes the inverse of the matrix and cache it.

cacheSolve <- function(x, ...) {
  output <- x$getinverse()	## Get the stored cache.
  if(!is.null(output)) {	## If it's not NULL, use its value, and then return it.
    message("getting cached data")
    return(output)
  }
  data <- x$get()	## Get the raw data.
  output <- solve(data, ...)	## Compute its inverse.
  x$setinverse(output)	## Store it in cache.
  output	## Return the computed inverse.
}