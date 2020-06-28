## Put comments here that give an overall description of what your
## functions do

## CACHING THE INVERSE OF AN INPUT MATRIX

makeCacheMatrix <- function(x = matrix()) {
  in_mat<- NULL        ## set the cache inverse to null
  set<- function(y) {  ##this function sets the input value to X object which is in global environment
    x <<- y
    in_mat <<- NULL
  }
  get <- function() x      ##displays the input matrix
  set_inv <- function(imat)  in_mat <<- imat   ##sets the cache inverse
  get_inv <- function() in_mat     ##displays the cache inverse matrix
  
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
  
}


##Computes the inverse of matrix and cache's it

cacheSolve <- function(x, ...) {
  
  in_mat <- x$get_inv()
  if(!is.null(in_mat)) {
    message("Getting the data cached")
  }
  d<- x$get()
  in_mat <- solve( d , ...)
  x$set_inv(in_mat)
  in_mat    ## Return a matrix that is the inverse of 'x'
}

