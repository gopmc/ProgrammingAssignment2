## R programming assingment 2
## Name: Oliver M. Christensen
## Change log: 
## 2014-12-17 Initial version downloaded 
## 2014-12-19 Mean code "template" rewritten from vector mean to inverse matrix
##            Tested and verified with known inversable 3x3 matrix:
##       [,1] [,2] [,3]                [,1]       [,2]        [,3]
## [1,]    1    4    7      [1,] -1.1428571  2.0000000  0.42857143
## [2,]    0    1    4  ->  [2,]  0.9523810 -1.6666667 -0.19047619
## [3,]    5    6    0      [3,] -0.2380952  0.6666667  0.04761905

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<- function(InputMatrix= matrix()) {
  ## Initialize data
  StoredMean <- NULL
  ## Define functions for getting and setting the global variable InputMatrix
  setMatrix <- function(y) {
    InputMatrix <<- y
    StoredMean <<- NULL
  }
  getMatrix <- function() InputMatrix
  
  ## Define functions for getting and setting the global variable InverseMatrix
  setInvMatrix <- function(CalculatedInMatrix) StoredMean <<- CalculatedInMatrix
  getInvMatrix <- function() StoredMean
  
  ## Return list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Calculates the inverse matrix from input matrix or return cashed inversed matrix
## if input matrix is the same as last time the function was called. 
## Input: InputMatrix. Type: Numerical, square and inversable matrix
## Returns: The calculated inverse of InputMatrix
cacheSolve <- function(InputCashObj, ...) 
{
  # Retrive cached inverse and check if data has changed
  CalcInvMatrix <- InputCashObj$getInvMatrix() #Reference into the recived function list.
  
  ## If input is unchanged, return the cashed result. 
  if(!is.null(CalcInvMatrix)) 
  {
    message("getting cached data")
    return(CalcInvMatrix)
  }
  
  ## Data has changed. Calculate new inverese matrix.
  NewInputMatrix <- InputCashObj$getMatrix()
  CalcInvMatrix <- solve(NewInputMatrix, ...)
  ## Update the cashed value and return the inverse Matrix
  InputCashObj$setInvMatrix(CalcInvMatrix)
  return(CalcInvMatrix)
}
