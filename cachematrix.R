## makeCacheMatrix & cacheSolve are two functions that cache the inverse of a matrix 

## This function creates a special "matrix" object; calculates its 
## inverse and sets the value of the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set() function: Sets a variable m to null & assigns the input matrix to x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get() function gets the matrix which is x
  get <- function() x 
  
  ## setinverse() function is the solve function. It assigns the inverse of the matrix to m
  setinverse <- function(solve) m <<- solve   
  
  ## getinverse() function gets the inverse of x which is now m
  getinverse <- function() m
  
  ## makeCachematrix is a list of 4 functions; set(), get(), setinverse(), getinverse()
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
}


## cacheSolve function checks if the inverse has already been calculated 
## (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.
## Otherwise cacheSolve calculates the inverse and caches it for later use

cacheSolve <- function(x, ...) {
  
  ## Gets the matrix inverse from makeCachematrix function and sets to m
  m <- x$getinverse()
  
  ## If matrix inverse is in cache then it is returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If matrix inverse not in cache then it is calculated and is set in cache as the m value 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Run function as follows
## Assign matrix to x 
x<- matrix(1:4, nrow=2,ncol=2)
a<-makeCacheMatrix(x)
cacheSolve(a)

## Running cacheSolve again will get cached inverse and message will display'
cacheSolve(a)