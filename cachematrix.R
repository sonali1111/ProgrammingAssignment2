## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function makeCacheMatrix does follwing:
#1. Gets a matrix as an input
#2. Set the value of the matrix
#3. Get the value of the matrix
#4. Set the inverse Matrix
#5. Get the inverse Matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
  
    #set the value of the Matrix
    set_matrix <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
  
  get_matrix <- function() x                              #Get the value of a Matrix
  set_inverse <- function(inverse) inv_matrix <<- inverse  #Set the value of an inverse matrix
  get_inverse <- function() inv_matrix                     #Get the value of an inverse matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse, get_inverse = get_inverse)
}
## The function cacheSolve does following:
#1. Takes the output of the previous matrix makeCacheMatrix(matrix) as an input
#2. Checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
#3. In case inverse matrix from makeCacheMatrix((matrix) is empty, 
# it gets the original matrix data and set the invertible  matrix by using the solve function.
#4. In case inverse matrix from makeCacheMatrix((matrix) has some value in it, 
# it returns a message  "Getting Cached Invertible Matrix" and the cached object

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")   #to display message
    return(inv_matrix)                             #return the inverse matrix
  }
  
  #if value of the invertible matrix is NULL then  
  matrix_data <- x$get_matrix()                     #get the original Matrix Data 
  inv_matrix <- solve(matrix_data, ...)             #use solve function to inverse the matrix
  x$set_inverse(inv_matrix)                         #set the inverse matrix 
  return(inv_matrix)                               #return the inverse matrix
}
