## These functions are designed to cache the inverse of a matrix 
## so that its value can be quickly retrieved after its been calculated once.
## 

## This function creates an object that keeps track of the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL 
  set <- function(y) { 
    x <<- y 
    m_inv <<- NULL
  } 
  
  get <- function() x 
  setminv <-function(solve) m_inv <<- solve 
  getminv <- function() m_inv 
  list(set = set, get = get, 
       setminv = setminv, 
       getminv = getminv)
}


## This function checks if the inverse has been cached. If so, passes the 
## cached solution. If not, calculates the inverse and sends that

cacheSolve <- function(w, ...) {
  m <- w$getminv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- w$get()
  m <- solve(data, ...)
  w$setminv(m)
  m
}
