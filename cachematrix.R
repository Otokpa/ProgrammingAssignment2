## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.


## The first function (makeCacheMatrix), is a function that: 
## 1.stores a matrix and its inverse, 
## 2.gives the possibility to set the matrix from an other environment, in witch case it resets the matrixInverse to NULL
## 3.gives the possibility to retreive the matrix and the inverse of the matrix from an other environment

makeCacheMatrix <- function(x = matrix()) {
matrixInverse <- NULL                                       ## reset matrixInverse to NULL when a matrix 
                                                            ## is set thru makeCacheMatrix
set <- function(y) {                                                      
  x <<- y                                                   ## set a matrix y from an other environment
  matrixInverse <<- NULL                                    ## reset to NULL so it can be updated later
}
get <- function() x                                         ## retreive the matrix 

setInverse <- function(inverse) {
  matrixInverse <<- inverse                                 ## store the inverse of the matrix from another environment
}

getInverse <- function() {
  matrixInverse                                             ## retreive the inverse of the matrix set aabove
}
list(set= set, get= get, 
     setInverse= setInverse, getInverse= getInverse)        ## the list of the functions
}


## cacheSolve is a function that checks if the inverse of the matrix cached in makeCacheMatrix is NULL in which case it
## calculates it and stores it in the function makeCacheMatrix. If the inverse is not NULL then it writes a message and
## returns the cached inverse of the matrix.

cacheSolve <- function(x, ...) {
  
  matrixInverse <- x$getInverse()                           ## retreives the inverse of the matrix from the 1st function
  if (!is.null(matrixInverse)) {                            ## checkes if the inverse is not NULL
    message('returning cached Inverse of the matrix')       ## in which case it returns a message
    return(matrixInverse)                                   ## returns the inverse matrix cached in the 1st function
  }
  
  matrixInverse <- solve(x$get())                           ## retreives the matrix from 1st function and calculates is inverse
  x$setInverse(matrixInverse)                               ## stores the calculated inverse of the matrix  in the 1st function
  return(matrixInverse)                                     ## Returns the calculated inverse of the matrix
}