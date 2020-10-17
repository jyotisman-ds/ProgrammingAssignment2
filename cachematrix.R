## Computing the inverse of a matrix and cache it if already computed once.

## makeCacheMatrix : this function returns a list to store functions which sets  
## and gets the value of the matrix and also functions to set and get the value
## of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve : this function takes as input the list from the above function
## and checks if the inverse of the input matrix is already computed. If not,
## it then computes the inverse and then caches the inverse value via the setinv
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
