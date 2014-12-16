# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly
# The next two functions enables caching in order to improve matrix inversion


# The first function, makeMatrix creates a special "matrix", which is really a list containing a function to
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse
#   get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


# The following function calculates the inverse of the special square "matrix" created 
# with the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  data <- x$get()
  
  #verify if the special "matrix" is square
  if(nrow(data) != ncol(data)){
    stop("In order to use solve function the matrix must be square (nrow = ncol)")
  }  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

########################################
##ALTERNATIVE FUNCTION - Extra challenge
########################################

library("MASS")
## The following function is a alternative to special square OR non square "matrix"
cacheGinv <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data, ...)
  x$setinverse(inv)
  inv
}


###############################
##  TEST CASES 
###############################

# x <- matrix(rnorm(100),10,10)
# m <- makeCacheMatrix(x)

## get x matrix 
# m$get() 

## No cache first run cacheSolve
# cacheSolve(m)

## Second run executes faster because is using cache data 
# cacheSolve(m)

###############################
## ALTERNATIVE TEST
###############################
## If we want to solve non squared matrix
## alternatively we can execute the following
# x <- matrix(rnorm(6),2,3)
# m <- makeCacheMatrix(x)
# cacheGinv(m)