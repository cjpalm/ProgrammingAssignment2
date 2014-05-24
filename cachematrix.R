## 2 functions that are used to create a special object that stores a square matirx
## and cache's its inverse. matrix is assumed to be square.
## 

## there are 4 functiosn in the first function.
## 1. sets the value of the matrix
## 2. get the value of the matirx
## 3. set the value of the inverse
## 4. get the value of the invers


makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
  
}


## cacheSolve returns inverse of a matix, first checks to see if inverse is cached, and returns
## that. If not cached, calculates inverse and returns that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
