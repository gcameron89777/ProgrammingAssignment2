## These functions cache the computed inverse of a given matrix. 
## If the value has already been calculated then the inverse of the matrix is retreived from the cache. If not then it is calculated and returned

## This function creates an object list that gets and sets the value of a matrix, then gets and sets the value of it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function takes the return value of makeCacheMatrix as an argument and returns the inverse of the matrix if already cached, else the inverse is computed and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
