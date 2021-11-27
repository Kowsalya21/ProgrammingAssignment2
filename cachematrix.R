## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           ## initializing inv as NULL       
  set <- function(y) {  ## defining the set function     
    x <<- y             ## value of matrix in parent environment
    inv <<- NULL        ## if there is a new matrix, reset inv to NULL              
    }
  get <- function() x   ## defining the get fucntion                  
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() 
    {
    inver <- ginv(x)
    inver%*%x   ## gets the value of inv
    }
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)  ## to refer to the functions with the $ operator
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {  #checking whether inverse is null
    message("getting cached data")
    return(inv)   ## Returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...) ##calculates inverse value
  x$setinverse(inv) 
  inv ##Return a matrix that is inverse of x
}

f <- makeCacheMatrix(matrix(1:8,2,4))
f$get()