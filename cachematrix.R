
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
# sets the value of m to NULL
  m <- NULL
  
# sets the value of y to NULL
  y <- NULL

# sets the value of the matrix
  setmatrix<-function(y){
    x<<-y
    m<<-NULL
  }

# gets the value of the matrix
  getmatrix<-function() x

# sets the inverse of the matrix using solve function
  setinverse<-function(solve) m<<- solve
# gets the inverse of the matrix
  getinverse<-function() m

# creates a list to cache the functions
  list(setmatrix=setmatrix, 
       getmatrix=getmatrix,
       setinverse=setinverse,
       getinverse=getinverse)  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then this function retrieves the inverse 
## from the cache.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'

  # Check if the inverse has already been calculated and cached
  # If cached, return cached matrix
  m<-x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  
  # Get value of input matrix
  matrix<-x$getmatrix()
  # Cache the input matrix
  x$setmatrix(matrix)
  # Calculate the inverse of the input matrix
  m<-solve(matrix, ...)
  # Cache the inverse matrix
  x$setinverse(m)
  m # return the inverse matrix
}
