## makeCacheMatrix & cacheSole are two functions that cache the inverse of a matrix 

## This function creates a special "matrix" object; calculates its 
## inverse and sets the value of the inverse in cache
'set() function: Sets a variable m to null & assigns the input matrix to x' 
'get() function gets the matrix which is x'
'setinverse() function is the solve function. It assigns the inverse of the matrix to m'
'getinverse() function gets the inverse of x which is now m'
'makeCachematrix is a list of 4 functions; set(), get(), setinverse(), getinverse()'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve   
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


'cacheSolve checks if the inverse has already been calculated 
(and the matrix has not changed), then cachesolve retrieves the inverse from the cache.
Otherwise cacheSolve calculates the inverse and caches it for later use'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

'Run function as follows'
'Assign matrix to x for example as below'

x<- matrix(1:4, nrow=2,ncol=2)
a<-makeCacheMatrix(x)
cacheSolve(a)
'Running cacheSolve again will get cached data and message will display'
cacheSolve(a)