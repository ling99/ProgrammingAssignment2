## To cache the inverse of matrix

## Create a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize inverse property
   
   inverseP <- NULL
   
   ## Method setting matrix
   
   set <- function(matrix) {
   
   x <<- matrix
   inverseP <<- NULL
  }
  
  ## Method getting matrix
  get <- function() x
  
  ## Method setting inverse of the matrix
  
  setInverse <- function(inverse) inverseP <<- inverse
  
  ## Method getting inverse of the matrix
  getInverse <- function() inverseP
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Compute the inverse of the matrix from makeCacheMatrix
## Retrieve the inverse of the matrix from cache if it is calculated before and unchanged

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  ## Return precalculated & unchanged matrix from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get matrix from the object
  data <- x$get()
 
  ##Calculate the inverse using matrix multiplication
  m <- solve(data)%*%data
  
  ##Set the inverse to the object
  x$setInverse(m)
  
  ##Return the matrix
  m
   
}
