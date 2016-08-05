##################################################################################
## Author: Joao Fernando S R de Mello                                           ##
## 05-August-2016 - Coursera R-Programming - Week-3 Assignment                  ##
##################################################################################

## Put comments here that give an overall description of what your functions do

## As matrix inversion can be a costly computation, this program catches the
## inverse rather, preventing the user to compute it repeatedly.


## Write a short comment describing this function
## makeCacheMatrix: 
##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initializing objects
    m <- NULL
    ## Define the 'set' function with the <<- operator, so that the right side object
    ##  is an object in the parent evironment
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    ## Define the 'get' function, refering the 'x' object from the parent evironment
    get <- function() x
    
    ## Define the 'setinv' function, refering all the objects in the parent evironment
    setinv <- function(myinverse) m <<- myinverse
    
    ## Define the 'getinv' function, wich returns the 'm' object from the parent evironment
    getinv <- function() m
    
    ## define the list with the 'getters' and 'setters' to be used in another evironment
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve: 
##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and the 
##  matrix has not changed), then the cachesolve should retrieve the inverse 
##  from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  ## gets the m object from the parent directory
  m <- x$getinv()
  
  ## if the 'm' object is not null, gets the cached object
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("calculating inverse matrix")
  
  ## getting the data from the parent evironment
  data <- x$get()
  
  ## Computing the inverse matrix
  m <- solve(data, ...)
  
  ## Setting the inverse matrix using the m object in the parent evironment
  x$setinv(m)
  
  ## Returning the inverse matrix
  m
    }
