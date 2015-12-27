## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix object that can cache the inverse of it
makeCacheMatrix <- function(x = matrix()) {      
  inverseM <- NULL #make inversed matrix NULL
  
  #set orginal matrix
  set <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  
  get <- function() x #get original matrix
  
  setInversed <- function(inverse) inverseM <<- inverse #set inversed matrix
  
  getInversed <- function() inverseM #get inversed matrix
  
  #return the list
  list(set=set, get=get, 
       setInversed =setInversed,
       getInversed=getInversed )
}


## Write a short comment describing this function

## Bring back a matrix that is the inverse of x
cacheSolve <- function(x, ...) {
  inversed <- x$getInversed()
  
  # when the inverse matrix is available, return it
  if(!is.null(inversed)){
    return(inversed)
  }
  
  # when the inverse matrix is not available, calculate it 
  originalMatrix <- x$get()
  inversed <- solve(originalMatrix)
  x$setInversed(inversed)
  inversed      
}
