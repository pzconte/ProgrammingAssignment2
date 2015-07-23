## Our functions will be caching the Inverse of a Matrix
## Example:
## > x = rbind(c(2, 1), c(5, 3))// A 2x2 Matrix
## > x
##      [,1] [,2]
## [1,]    2    1
## [2,]    5    3
## > i = makeCacheMatrix(x) 		// create the special Matrix
## > i$get()			            	// The Matrix
##     [,1] [,2]
## [1,]    2    1
## [2,]    5    3
## > cacheSolve(i)			       // Call Inverse of Matrix i for the first time
##     [,1] [,2]
## [1,]    3   -1
## [2,]   -5    2
## > cacheSolve(i)			        //Call again the inverse of Matrix i
## getting cached data			    // we got the cached inverse
##     [,1] [,2]
## [1,]    3   -1
## [2,]   -5    2


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) i <<- inverse
  getinverse <- function(inverse) i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  #To get the inverse of a Matrix, we must use the solve function
  i <- solve(data)
  x$setinverse(i)
  i
}
