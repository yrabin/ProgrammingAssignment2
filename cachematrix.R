## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly (there are also alternatives
## to matrix inversion that we will not discuss here).
## This assignment is to write a pair of functions that cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL

  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }

  get <- function() x

  setinverse <- function(inverse) inverseX <<- inverse

  getinverse <- function() inverseX

  #return a list of functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invX <- x$getinverse()

  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }

  data <- x$get()
  invX <- solve(data, ...)
  x$setinverse(invX)
  invX
}



## TEST CODE
"nrows <- 2 #must be square matrix!
ncols <- nrows
myMatrix <- matrix(runif(ncols*nrows), ncol=ncols) #generate random matrix
print(myMatrix)

cacheMatrix <- makeCacheMatrix(myMatrix)
invSolution <- cacheSolve(cacheMatrix) #first run will cache the inverse result
print(invSolution)

scndInvSolution <- cacheSolve(cacheMatrix) #2nd run, results from cache
print(scndInvSolution)"

