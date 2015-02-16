## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix( c(1,0,0,1), nrow=2) ) {
  ## 'x' is a quadratic, invertible matrix or the identitymatrix by default
  
  i <- NULL  ## assign 'i' to NULL
  
  set <- function(y) {    
    x <<- y     ## 'y' is assigned to 'x' through parent environment  
    i <<- NULL  ## 'i' is assigned to 'NULL' through parent environment
  }
  
  get <- function() x  ## returns the matrix 'x'
  
  setinverse <- function(inverse) i <<- inverse   ## 'i' is assigned to 'inverse' through parent environment
  getinverse <- function() i                      ## returns the matrix 'inverse' which was set by 'setinverse()' or NULL
  
  
  ## returns a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(mcm, ...) {
  ## 'mcm' is a makeCacheMatrix object
  
  ## call 'getinverse()' function on the makeCacheMatrix object 'mcm' and assign it to 'myinverse'
  myinverse <- mcm$getinverse()
  
  if(!is.null(myinverse)) {            ## if 'myinverse' is not null
    message("getting cached data")
    return(myinverse)                  ## return 'myinverse'
  }
  
  ## else if 'myinverse' is null
  ## call 'get()' function on the makeCacheMatrix object 'mcm' to get the original matrix
  data <- mcm$get()
  
  ## invert the original matrix and assign it to 'myinverse'
  myinverse <- solve(data, ...)
  
  ## call 'setinverse()' function on the makeCacheMatrix object 'mcm' + 
  ## to set the calculated inverse of the original matrix 'x'
  mcm$setinverse(myinverse)
  
  ## return myinverse
  myinverse
}
