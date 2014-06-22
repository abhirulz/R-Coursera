
## makeCacheMatrix creates a matrix object 

makeCacheMatrix <- function(x = matrix()) {
  xmat_inv = NULL
  
  #Set Matrix function
  set <- function(y) {
    x <<- y
    xmat_inv <<- NULL
  }
  
  #Get Matrix function
  get <- function() x
  
  #Set inverse of matrix function
  setinverse<- function(inverse) xmat_inv <<-inverse
  
  #Get Inverse of matrix function
  getinverse <- function() xmat_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns inverse of matrix created by makeCacheMatrix function.
## If inverse is avaialbe in cache, cacheSolve function retreives it and 
## if cache is empty, the function computes it sets the cache and returns it 

cacheSolve <- function(x) {
  
  ## Returns the inverse of matrix x 
  xinv = x$getinverse()
  
  #Check if cache has any value
  if (!is.null(xinv)) {
    message("displaying cached inverse of the matrix")
    return(xinv)
  } 
  else {
    xinv = solve(x$get())
    x$setinverse(xinv)
    return(xinv)
  }
}
 
# g=makeCacheMatrix(matrix(rnorm(1000000),1000,1000))
# cacheSolve(g)
# 
# p=solve(matrix(rnorm(1000000),1000,1000))


