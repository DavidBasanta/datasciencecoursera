## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##I did it before each order
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property  
  inv <- NULL
  
  ## Set the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix 
  get <- function() x
  
  ## Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the inverse 
  getinverse <- function() inv
  
  ##show the list of the methods
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##I did it before each order

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## return the inverse (only if its already set)
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ##Obtain a matrix from my object
  data <- x$get()
  
  ##calculate the inverse
  inv <- solve(data)
  
  ##do inverse
  x$setinverse(inv)
  
  ##return the matrix
  inv
}
