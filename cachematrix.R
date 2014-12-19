## R programming assingment 2
## Name: Oliver M. Christensen
## Change log: 
## 2014-12-17 Initial version downloaded 
## 2014-12-19 Mean code "template" rewritten from vector mean to inverse matrix 

## This function creates a special "matrix" object that can cache its inverse.
 <- function(x = matrix()) {
  ## Initialize data
  m <- NULL
  ## Define functions for getting and setting the global variable InputMatrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Define functions for getting and setting the global variable InverseMatrix
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  
  ## Return list of functions
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## Calculates the inverse matrix from input matrix or return cashed inversed matrix
## if input matrix is the same as last time the function was called. 
## Input: InputMatrix. Type: Numerical, square and inversable matrix
## Returns: The calculated inverse of InputMatrix
cacheSolve <- function(x, ...) 
{
  # Retrive cached inverse and check if data has changed
  m <- x$getmean()
  
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  ## Data has changed. Calculate new inverese matrix.
  data <- x$get()
  m <- mean(data, ...)
  ## Update the cashed value and return the inverse Matrix
  x$setmean(m)
  return(m)
}
