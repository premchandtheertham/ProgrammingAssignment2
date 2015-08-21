## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(num_1 = matrix()) {
  # Storing the Inverse
  invers <- NULL
  
  # TO Alter the matrix, use "var_1"
  var_1 <- function(num_2) {
    num_1 <<- num_2
    invers <<- NULL
  }
  
  # Returning the matrix using "var_2"
  var_2 <- function() {
    num_1
  }
  
  # setting_invers sets the invers variable, and this will be used only in cacheSolve
  setting_invers <- function(i) {
    invers <<- i
  }
  
  # To gets the cached inverse , "getting_invers"
  getting_invers <- function() {
    invers
  }
  
  # Special Matrix return below
  list(set = var_1,
       get = var_2,
       set = setting_invers,
       get = getting_invers)    
}

## Write a short comment describing this function
## Using the special matrix derved in the previously , Compute the inverse.
## IF the matrix is unchanged , then cacheSolve will retive the inverse

cacheSolve <- function(num_1, ...) {
  # get the cached inverse
  invers <- num_1$get()
  
  if(!is.null(invers)) {
    # if the inverse if actually cached, just return it
    message("Cached inverse")
    return(invers)
  }
  
  # otherwise, calculate the inverse and cache it
  matr <- num_1$get()
  invers <- solve(matr, ...)
  num_1$set(invers)
  
  return(invers)
}


## Sample Test 1
# check <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(check)
# cacheSolve(check)  ## "cached inverse"
# check$set(matrix(5:8, 2, 2))
# cacheSolve(check)
# cacheSolve(check)  ## "cached inverse"

## Sample Test 2:
# check2 <- makeCacheMatrix(matrix(4:7, 2, 2))
# cacheSolve(check2)
# cacheSolve(check2)  ## "cached inverse"
# check2$set(matrix(8:11, 2, 2))
# cacheSolve(check2)
# cacheSolve(check2)  ## "cached inverse"

## Sample Test 3:
# check3 <- makeCacheMatrix(matrix(1:8, 4, 4))
# cacheSolve(check3)
# cacheSolve(check3)  ## "cached inverse"
# check3$set(matrix(8:15, 4, 4))
# cacheSolve(check3)
# cacheSolve(check3)  ## "cached inverse"

## Sample Test 4:
# check4 <- makeCacheMatrix(matrix(1:6, 3, 3))
# cacheSolve(check4)
# cacheSolve(check4)  ## "cached inverse"
# check4$set(matrix(6:11, 3, 3))
# cacheSolve(check4)
# cacheSolve(check4)  ## "cached inverse"
