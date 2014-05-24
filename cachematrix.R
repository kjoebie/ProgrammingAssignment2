## Put comments here that give an overall description of what your
## functions do


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # init symbol cachedvalue
  # this symbol will hold the cached value of the matrix
  cachedvalue <- NULL
  
  #This function sets the value of the matrix
  set <- function(y) {
    # assign arugment y to the value of x
    # x is assigned in another environment so the <<- operator is used
    x <<- y

    #clears the cache
    cachedvalue <<- NULL
  }
  
  # get returns the value of the matrix to the caller
  # x holds the value of the matrix
  get <- function() x
  
  # setinv stores the value of the matrix in the cache
  # because the symbol cachedvalue is defined in another environment (makeCacheMatrix) 
  # the <<- operator is used
  setinv <- function(matrix) cachedvalue <<- matrix
  
  #getinv returns the value of the cache
  getinv <- function() cachedvalue
  
  #exposes the functions to the outside world
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## The function cacheSolve computes the inverse of the special "matrix" returned by the function makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve function
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # call the getinv funcion on the cachematrix to inverse the matrix
  m <- x$getinv()
  
  # if it returns a value then this means there is a value in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #
  # No value in the cache so calculate the inverse
  #
  
  #retrieve the matrix from the cachematrix
  data <- x$get()

  #inverse the matrix
  m <- solve(data)
  
  #store the result in the cachematrix
  x$setinv(m)

  #return the result 
  m
}
