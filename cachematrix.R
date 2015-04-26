## functions to create vector and cache time consuming computations

##This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}

##Test
##> x = rbind(c(2, 3), c(4, 5))
##> i = makeCacheMatrix(x)
##> i$get()
##[,1] [,2]
##[1,]    2    3
##[2,]    4    5
##> cacheSolve(i)
##[,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0
##> cacheSolve(i)
##getting cached data
##[,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0

