## The following function create a special "matrix" object that can cache its inverse; which is a list containing 
# 1.set the value of matrix
# 2.Get the value of matrix
# 3.set the value of inverse matrix
# 4.Get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y){
    x <<- y
    invm <<- NULL 
  }
  get <- function() x
  setinversematrix <- function(invermatrix) invm <<- invermatrix
  getinversematrix <- function() invm
  list(set = set,get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}


## The following function compute the inverse of the special "matrix" created by above function. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  invm <- x$getinversematrix()
  if(!is.null(invm)) {                
    message("getting cached data")
    return(invm)                     ## Return a matrix that is the inverse of 'x'
  }  
  data <- x$get()
  invm <- solve(data, ...)
  x$setinversematrix(invm)
  invm
}
