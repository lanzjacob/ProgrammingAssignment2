## The functions below perform the caching of the inverse of a matrix

## The function makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to: set and get the matrix, as well as,
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  #Set the Matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #Get the Matrix
  get <- function() x
  
  #Set the Inverse of the Matrix
  setInverse <- function(inverse) i <<- inverse
 
  #Get the Inverse of the Matrix
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function cacheSolve obtains the inverse of the special "matrix" created
## with the above function. It first checks if the inverse was already previously
## obtained. If so, it gets the inverse from the cache and skips the calculation.
## Otherwise, it gets the inverse of the matrix and sets the value of the inverse
## in the cache via the setInverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  #Check if the inverse is already set. If yes, return the inverse
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  #Get the Matrix
  data <- x$get()
  
  #Obtain the Inverse of the Matrix
  i <- solve(data, ...)
  
  ##Set the Inverse of the Matrix
  x$setInverse(i)
  
  ##Return the Matrix
  i
}
