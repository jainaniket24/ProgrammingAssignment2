## The two functions here perform matrix inversion using solve()
## function. First function returns a list of getter and setter 
## functions for the matrix and its inverse; while the second
## function checks if there is a inverse already set in the cache;
## returns it if found; else computes a fresh inverse


## This function accepts a sqaure matrix as its input,
## and retunrs a list of following functions:
## 1. set_m: for setting the value of the matrix
## 2. get_m: for retrieving the matrix
## 3. set_inv: for setting the inverse of matrix
## 4. get_inv: for retrieving the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_m <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get_m <- function(){
    x
  }
  set_inv <- function(matrix_inv){
    inverse <<- matrix_inv
  }
  get_inv <- function(){
    inverse
  }
  list(set=set_m, get=get_m, setinverse=set_inv, getinverse=get_inv)
}


## This function accepts a matrix object as input, and returns inverse
## of this matrix. It first checks if there is a cached value of inverse
## and returns if it finds one; else does a fresh calculation to 
## compute the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Returning cached value of inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat) ## Assuming matrix supplied is always square invertible matrix
  x$setinverse(inv)
  inv
}
